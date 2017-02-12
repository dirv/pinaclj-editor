(ns pinaclj-editor.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]
            [pinaclj-editor.dom :as pdom]
            [pinaclj-editor.caret :as caret]
            [pinaclj-editor.text-node-navigator :as text]
            [pinaclj-editor.validity :as validity])
  (:import [goog.events KeyCodes]))

(enable-console-print!)

(def modifier-mappings
  {[(.-B KeyCodes) :meta] [dom/TagName.STRONG]
   [(.-I KeyCodes) :meta] [dom/TagName.EM]
   [(.-C KeyCodes) :meta] [dom/TagName.CITE]
   [(.-ONE KeyCodes) :shift :meta] [dom/TagName.H1]
   [(.-TWO KeyCodes) :shift :meta] [dom/TagName.H2]
   [(.-THREE KeyCodes) :shift :meta] [dom/TagName.H3]
   [(.-FOUR KeyCodes) :shift :meta] [dom/TagName.H4]
   [(.-FIVE KeyCodes) :shift :meta] [dom/TagName.H5]
   [(.-SIX KeyCodes) :shift :meta] [dom/TagName.H6]
   [(.-O KeyCodes) :meta] [dom/TagName.OL dom/TagName.LI]
   })

(def selection-mappings
  {[(.-LEFT KeyCodes) :shift] :character-left
   [(.-RIGHT KeyCodes) :shift] :character-right
   [(.-LEFT KeyCodes) :alt :shift] :word-left
   [(.-RIGHT KeyCodes) :alt :shift] :word-right
   [(.-LEFT KeyCodes) :shift :meta] :line-left
   [(.-RIGHT KeyCodes) :shift :meta] :line-right})

(def movement-mappings
  {[(.-LEFT KeyCodes)] :move-character-left
   [(.-RIGHT KeyCodes)] :move-character-right
   [(.-LEFT KeyCodes) :alt] :move-word-left
   [(.-RIGHT KeyCodes) :alt] :move-word-right
   [(.-LEFT KeyCodes) :meta] :move-line-left
   [(.-RIGHT KeyCodes) :meta] :move-line-right
   })

(defn- move-caret [root [start-container start-offset end-container end-offset] movement-type]
  (caret/keep-within
    root
    (apply caret/caret-at
           (case movement-type
             :move-character-left
             (text/character-left start-container start-offset)
             :move-character-right
             (text/character-right end-container end-offset)
             :move-word-left
             (text/word-left start-container start-offset)
             :move-word-right
             (text/word-right end-container end-offset)))))

; todo: append is probably not the right thing, need to choose the node after the current node passed in, rather than the end node
; todo: retags should only choose phrasing content
; todo: this needs to delete any empty previous tags
(defn- open-new-tag [text-node [tag-name :as tag-names]]
  (let [insert-point (validity/find-insert-point (.-parentElement text-node) tag-name)
        re-tags (filter (partial validity/is-valid-child? tag-name)
                        (pdom/tags-between text-node insert-point))]
    (when-not (= (.-parentElement text-node) insert-point)
      (pdom/remove-empty-nodes text-node))
    (pdom/append-tree insert-point (concat tag-names re-tags [""]))))

(defn- close-existing-tag [current existing]
  (pdom/insert-tree-after existing (pdom/tags-between current existing)))

(def default-structural-nodes ["P" ""])

(defn- close-list [list-element li-element re-tags]
  (pdom/remove-node li-element)
  (let [text-node (pdom/insert-tree-before (.-parentElement list-element)
                                           (.-nextSibling list-element)
                                           (concat re-tags default-structural-nodes))]
    (if (= 0 (count (.-children list-element)))
      (pdom/remove-node list-element))
    (caret/place-caret text-node)))

(defn- reparent-next-siblings [search-node old-parent new-parent]
  (doseq [sibling (vec (pdom/all-siblings-after old-parent search-node))]
    (pdom/reparent new-parent sibling)))

(defn- move-next-siblings [new-parent node-hierarchy]
  (dorun (map (partial reparent-next-siblings (first node-hierarchy))
    node-hierarchy
    (pdom/node-path new-parent))))

; note, this could also work for li potentially
(defn- break-at-enter [breaker [text-node position] re-tags]
  (let [new-text-node (.splitText text-node position)
        deepest-child (pdom/insert-tree-after breaker re-tags)]
    (move-next-siblings deepest-child (pdom/nodes-between-inclusive text-node breaker))
    (caret/place-caret new-text-node)))

(defn- handle-enter [_]
  (let [caret (caret/delete-range) ; todo- is this the right place for it?
        text-node (nth caret 2)
        breaker (validity/find-breaking-element text-node)
        tags-between (pdom/tags-between text-node breaker)]
    (cond
      (= dom/TagName.OL (.-tagName breaker))
      (let [li-element (pdom/find-tag text-node dom/TagName.LI)]
        (if (pdom/node-empty? text-node)
          (close-list breaker li-element (pdom/tags-between text-node li-element))
          (break-at-enter breaker caret (cons "LI" tags-between))))
      :else
      (break-at-enter breaker caret (cons "P" tags-between))))) ; todo - use inclusive?

; todo - this could be used for the delete key too...
(defn- delete-one-character [root caret]
  (when-let [new-caret (move-caret root caret :move-character-left)]
    (when (not= new-caret caret)
      (println "moved to " (.-textContent (first new-caret)) (second new-caret))
      (.deleteData (first new-caret) (second new-caret) 1)
      (if-not (= (first new-caret) (first caret))
        (pdom/remove-empty-nodes (first caret))))
    new-caret))

(defn- insert-backspace [root [start-container start-offset end-container end-offset :as caret]]
  (if (caret/selection? caret)
    (caret/delete-range)  ; todo - need to ensure that the range is within our own document
    (delete-one-character root caret)))

(defn- insert-character [[start-container start-offset end-container end-offset] c]
  (.insertData end-container end-offset c)
  (caret/caret-at end-container (inc end-offset)))

; todo - needs to change to take into account left or right side of caret.
(defn- extend-range [[start-container start-offset end-container end-offset] selection-type]
  (case selection-type
      :word-left
      (concat (text/word-left start-container start-offset) [end-container end-offset])
      :word-right
      (concat [start-container start-offset] (text/word-right end-container end-offset))
      :character-left
      (concat (text/character-left start-container start-offset) [end-container end-offset])
      :character-right
      (concat [start-container start-offset] (text/character-right end-container end-offset))
      :line-left
      (println "not implemented yet")
      :line-right
      (println "not implemented yet")
      :else
      (println "not implemented yet")))

; todo - this should work on selections too
(defn- decorate-nodes [tags caret]
  (when-not (caret/selection? caret)
    (caret/place-caret
      (let [element (first caret)]
      (if-let [existing (pdom/find-tag element (first tags))]
        (close-existing-tag element existing)
        (open-new-tag element tags))))))

; todo - delete selection first
(defn- perform-action [root [c & modifiers :as key-desc] caret]
  (println "Handling key combo" key-desc)
  (cond
    (= c (.-BACKSPACE KeyCodes))
    (insert-backspace root caret)
    (= c (.-ENTER KeyCodes))
    (handle-enter caret)
    (and (contains? movement-mappings key-desc))
    (move-caret root caret (get movement-mappings key-desc))
    (and (contains? selection-mappings key-desc))
    (extend-range caret (get selection-mappings key-desc))
    (and (contains? modifier-mappings key-desc))
    (decorate-nodes (get modifier-mappings key-desc) caret)))

(defn- ->char [[c & modifiers]]
  (let [ch (char c)]
    (if (some #{:shift} modifiers)
      ch
      (.toLowerCase ch))))

(defn- print-character [[c & modifiers :as key-desc] _]
  (println "Printing character " key-desc)
  (when-not (some #{:meta :alt} modifiers)
    (let [caret (caret/delete-range)]
      (insert-character caret (->char key-desc)))))

(defn- modifier-map [e]
  {:alt (.-altKey e)
   :ctrl (.-ctrlKey e)
   :shift (.-shiftKey e)
   :meta (.-metaKey e)})

(defn- modifiers-of [e]
  (mapv first (filter second (modifier-map e))))

(defn- handle-keypress [root e]
  (when (caret/do-update (partial print-character (cons (.-charCode e) (modifiers-of e))))
    (.preventDefault e)))

(defn- handle-keydown [root e]
  (when (caret/do-update (partial perform-action root (cons (.-keyCode e) (modifiers-of e))))
    (.preventDefault e)))

(defn- handle-mouseup [root e]
  (caret/do-update (partial println)))

(defn- initialize-document [root _]
  (apply caret/caret-at
    (or (first (text/forwards-caret-traversal root 0))
        [(pdom/append-tree root default-structural-nodes) 0])))

(defn- edit [root]
  (caret/do-update (partial initialize-document root))
  (events/listen (dom/getDocument) "keydown" (partial handle-keydown root))
  (events/listen (dom/getDocument) "keypress" (partial handle-keypress root))
  (events/listen (dom/getDocument) "mouseup" (partial handle-mouseup root)))

(edit (dom/getElement "editor"))
