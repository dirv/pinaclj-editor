(ns pinaclj-editor.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]
            [pinaclj-editor.caret :as caret]
            [pinaclj-editor.text-node-navigator :as text]
            [pinaclj-editor.validity :as validity])
  (:import [goog.events KeyCodes]))

(enable-console-print!)

(defn- insert-after [before element]
  (.insertBefore (.-parentElement before) element (.-nextSibling before)))

(defn- create-tree [[tag & tags]]
  (let [elem (.createElement js/document tag)]
    (when (seq? tags)
      (.appendChild elem (create-tree tags)))
    elem))

(defn- append-child [parent child]
  (.appendChild parent child)
  child)

(defn- deepest-child [node]
  (if-let [child (.-firstChild node)]
    (deepest-child child)
    node))

(defn- append-tree [parent tags]
  (let [children (create-tree tags)]
    (append-child parent children)
    (deepest-child children)))

(defn- insert-tree-before [parent sibling tags]
  (let [children (create-tree tags)]
    (.insertBefore parent children sibling)
    (deepest-child children)))

(defn- find-tag [root leaf tag-name]
  (cond
    (= root leaf)
    nil
    (= (.-tagName leaf) tag-name)
    leaf
    :else
    (find-tag root (.-parentElement leaf) tag-name)))

(defn- tags-between [current elem]
  (let [this-tag (.-tagName current)]
    (cond
      (= (.-tagName current) (.-tagName elem))
      []
      (= nil (.-tagName current))
      (tags-between (.-parentElement current) elem)
      :else
      (conj (tags-between (.-parentElement current) elem) this-tag))))

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
  (apply caret/caret-at
    (case movement-type
      :move-character-left
      (text/character-left root start-container start-offset)
      :move-character-right
      (text/character-right root end-container end-offset)
      :move-word-left
      (text/word-left root start-container start-offset)
      :move-word-right
      (text/word-right root end-container end-offset))))

(defn- append-text-node [parent]
  (caret/place-caret (append-child parent (.createTextNode js/document ""))))

(defn- insert-text-node-after [current]
  (let [node (dom/createTextNode "")]
    (insert-after current node)
    (caret/place-caret node)))

(defn- node-path [root node]
  (if (= root node)
    []
    (cons node (node-path root (.-parentElement node)))))

(defn- node-empty? [node]
  (= 0 (count (dom/getTextContent node))))

(defn- all-empty [root current]
  (take-while node-empty? (node-path root current)))

(defn- remove-empty-nodes [root text-node]
  (if-let [empty-tree (seq (all-empty root text-node))]
    (dom/removeNode (last empty-tree))))

; todo: append is probably not the right thing, need to choose the node after the current node passed in, rather than the end node
; todo: retags should only choose phrasing content
; todo: this needs to delete any empty previous tags
(defn- open-new-tag [root text-node [tag-name :as tag-names]]
  (let [insert-point (validity/find-insert-point root (.-parentElement text-node) tag-name)
        re-tags (filter (partial validity/is-valid-child? tag-name)
                        (tags-between text-node insert-point))]
    (when-not (= (.-parentElement text-node) insert-point)
      (remove-empty-nodes insert-point text-node))
    (append-text-node (append-tree insert-point (concat tag-names re-tags)))))

(defn- close-existing-tag [root current existing]
  (let [re-tags (tags-between current existing)]
    (if (empty? re-tags)
      (insert-text-node-after existing)
      (append-text-node (append-tree (validity/find-insert-point root (.-parentElement existing) (first re-tags)) re-tags)))))

(def default-structural-element dom/TagName.P)

(defn- insert-default-structural-element [parent after-sibling tags-between]
  (append-text-node (insert-tree-before parent after-sibling (cons default-structural-element tags-between))))

(defn- close-list [list-element li-element re-tags]
  (.removeNode li-element)
  (let [caret (insert-default-structural-element (.-parentElement list-element) (.-nextSibling list-element) re-tags)]
    (if (= 0 (count (.-children list-element)))
      (.removeNode list-element))
    caret))

(defn- siblings [first-child]
  (if first-child
    (cons first-child (siblings (.-nextSibling first-child)))
    '()))

(defn- all-siblings-after [breaker node]
  (rest (drop-while #(not (.contains % node)) (siblings (.-firstChild breaker)))))

(defn- break-at-enter [breaker [text-node position] re-tags]
  (let [next-nodes (all-siblings-after breaker text-node)
        new-text-node (.splitText text-node position)]
    (dom/removeNode new-text-node)
    (doall (map dom/removeNode next-nodes))
    (let [new-parent-node (insert-after breaker (create-tree (cons "P" re-tags)))]
      (.appendChild (deepest-child new-parent-node) new-text-node)
      (doall (map #(.appendChild new-parent-node %) next-nodes))
      (caret/place-caret new-text-node))))

(defn- handle-enter [root _]
  (let [caret (caret/delete-range)
        text-node (nth caret 2)
        breaker (validity/find-breaking-element root text-node)
        tags-between (tags-between text-node breaker)]
    (cond
      (= dom/TagName.OL (.-tagName breaker))
      (let [li-element (find-tag breaker text-node dom/TagName.LI)]
        (if (node-empty? text-node)
          (close-list breaker li-element (tags-between text-node li-element))
          (append-text-node (insert-tree-before breaker (.-nextSibling li-element) tags-between))))
      :else
      (break-at-enter breaker caret tags-between))))

; todo - not sure if that works correctly across all nodes now
(defn- delete-one-character [root caret]
  (when-let [new-caret (move-caret root caret :move-character-left)]
    (.deleteData (first new-caret) (second new-caret) 1)
    (if-not (= (first new-caret) (first caret))
      (remove-empty-nodes root (first caret)))
    new-caret))

(defn- insert-backspace [root [start-container start-offset end-container end-offset :as caret]]
  (if (caret/selection? caret)
    (caret/delete-range)
    (delete-one-character root caret)))

(defn- insert-character [root [start-container start-offset end-container end-offset] c]
  (.insertData end-container end-offset c)
  (caret/caret-at end-container (inc end-offset)))

; todo - needs to change to take into account left or right side of caret.
(defn- extend-range [root [start-container start-offset end-container end-offset] selection-type]
  (case selection-type
      :word-left
      (concat (text/word-left root start-container start-offset) [end-container end-offset])
      :word-right
      (concat [start-container start-offset] (text/word-right root end-container end-offset))
      :character-left
      (concat (text/character-left root start-container start-offset) [end-container end-offset])
      :character-right
      (concat [start-container start-offset] (text/character-right root end-container end-offset))
      :line-left
      (println "not implemented yet")
      :line-right
      (println "not implemented yet")
      :else
      (println "not implemented yet")))

; todo - this should work on selections too
(defn- decorate-nodes [root tags caret]
  (when-not (caret/selection? caret)
    (let [element (first caret)]
      (if-let [existing (find-tag root element (first tags))]
        (close-existing-tag root element existing)
        (open-new-tag root element tags)))))

; todo - some of these may require deleting text in caret first.
; take into account the ones that delete text (backspace, enter)
; maybe backspace and enter can actually be done in insert-charater?
(defn- perform-action [root [c & modifiers :as key-desc] caret]
  (println "Handling key combo" key-desc)
  (cond
    (= c (.-BACKSPACE KeyCodes))
    (insert-backspace root caret)
    (= c (.-ENTER KeyCodes))
    (handle-enter root caret)
    (and (contains? movement-mappings key-desc))
    (move-caret root caret (get movement-mappings key-desc))
    (and (contains? selection-mappings key-desc))
    (extend-range root caret (get selection-mappings key-desc))
    (and (contains? modifier-mappings key-desc))
    (decorate-nodes root (get modifier-mappings key-desc) caret)))

(defn- ->char [[c & modifiers]]
  (let [ch (char c)]
    (if (some #{:shift} modifiers)
      ch
      (.toLowerCase ch))))

; todo: ensure caret moves properly
; todo: ensure that this caret is a text-node.
(defn- print-character [root [c & modifiers :as key-desc] _]
  (println "Printing character " key-desc)
  (when-not (some #{:meta :alt} modifiers)
    (let [caret (caret/delete-range)]
      (insert-character root caret (->char key-desc))
      (move-caret root caret :move-character-right))))

(defn- modifier-map [e]
  {:alt (.-altKey e)
   :ctrl (.-ctrlKey e)
   :shift (.-shiftKey e)
   :meta (.-metaKey e)})

(defn- modifiers-of [e]
  (mapv first (filter second (modifier-map e))))

(defn- handle-keypress [root e]
  (when (caret/do-update (partial print-character root (cons (.-charCode e) (modifiers-of e))))
    (.preventDefault e)))

(defn- handle-keydown [root e]
  (when (caret/do-update (partial perform-action root (cons (.-keyCode e) (modifiers-of e))))
    (.preventDefault e)))

(defn- handle-mouseup [root e]
  (println "Caret: " )
  (caret/do-update (partial println)))

(defn- initialize-document [root _]
  (insert-default-structural-element root nil []))

(defn- edit [root]
  (caret/do-update (partial initialize-document root))
  (events/listen (dom/getDocument) "keydown" (partial handle-keydown root))
  (events/listen (dom/getDocument) "keypress" (partial handle-keypress root))
  (events/listen (dom/getDocument) "mouseup" (partial handle-mouseup root)))

(edit (dom/getElement "editor"))
