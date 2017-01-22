(ns pinaclj-editor.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]
            [pinaclj-editor.selection :as selection]
            [pinaclj-editor.text-node-navigator :as text]
            [pinaclj-editor.validity :as validity])
  (:import [goog.events KeyCodes]))

(enable-console-print!)

(defn- insert-after [current tag]
  (let [elem (dom/createElement tag)
        text (dom/createTextNode "")]
    (dom/insertSiblingAfter elem current)
    (dom/append elem text)
    text))

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

(defn- find-tag [root current tag-name]
  (cond
    (= root current)
    nil
    (= (.-tagName current) tag-name)
    current
    :else
    (find-tag root (.-parentElement current) tag-name)))

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

(defn- create-range []
  (.createRange js/document))

(defn- add-range [rng sel]
  (.addRange sel rng)
  rng)

(defn- get-range []
  (let [sel (.getSelection js/window)]
    (if (zero? (.-rangeCount sel))
      (-> (create-range) (add-range sel))
      (.getRangeAt sel 0))))

(defn- new-caret-boundary [root rng movement-type]
  (case movement-type
    :move-character-left
    (text/character-left root (.-startContainer rng) (.-startOffset rng))
    :move-character-right
    (text/character-right root (.-endContainer rng) (.-endOffset rng))
    :move-word-left
    (text/word-left root (.-startContainer rng) (.-startOffset rng))
    :move-word-right
    (text/word-right root (.-endContainer rng) (.-endOffset rng))))

(defn- move-caret [root current movement-type]
  (let [rng (get-range)]
    (when-let [new-caret (new-caret-boundary root rng movement-type)]
      (apply #(.setStart rng %1 %2) new-caret)
      (.collapse rng true)
      (first new-caret))))

(defn- place-caret [text-node]
  (let [rng (get-range)]
    (.setStart rng text-node 0)
    (.collapse rng true)
    text-node))

(defn- insert-text-node [parent]
  (place-caret (append-child parent (.createTextNode js/document ""))))

(defn- insert-text-node-after [current]
  (let [node (dom/createTextNode "")]
    (dom/insertSiblingAfter node current)
    (place-caret node)))

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
    (insert-text-node (append-tree insert-point (concat tag-names re-tags)))))

(defn- close-existing-tag [root current existing]
  (let [re-tags (tags-between current existing)]
    (if (empty? re-tags)
      (insert-text-node-after existing)
      (insert-text-node (append-tree (validity/find-insert-point root (.-parentElement existing) (first re-tags)) re-tags)))))

;; todo this needs to be fixed
; any tags between would need to be cloned
(defn- close-list [list-element current re-tags]
  (dom/removeNode current)
  (let [new-parent (insert-text-node (append-tree (.-parentElement list-element) (cons dom/TagName.P re-tags)))]
    (if (= 0 (count (dom/getTextContent current)))
      (dom/removeNode list-element))
    new-parent))

(def default-structural-element dom/TagName.P)

(defn- append-default-structural-element [node tags-between]
  (insert-text-node (append-tree node (cons default-structural-element tags-between))))

(defn- handle-enter [root text-node]
  (let [breaker (validity/find-breaking-element root text-node)
        tags-between (tags-between text-node breaker)]
    (cond
      (= dom/TagName.OL (.-tagName breaker))
      (let [li-element (find-tag breaker text-node dom/TagName.LI)]
        (if (node-empty? text-node)
          (close-list breaker li-element (tags-between text-node li-element))
          (insert-text-node (append-tree breaker tags-between))))
      :else
      (append-default-structural-element (.-parentElement breaker) tags-between))))

(defn- insert-backspace [root text-node]
  (if-let [new-boundary (move-caret root text-node :move-character-left)]
    (let [rng (get-range)]
      (do (.deleteData (.-startContainer rng) (.-startOffset rng) 1)
        (if-not (= new-boundary text-node)
          (remove-empty-nodes root text-node))
        new-boundary))
    text-node))

; todo - this might be the place to move the caret on, since we deal with it here
;        but do delete of existing range content first.
(defn- insert-character [root current c]
  (let [rng (get-range)
        node (.-startContainer rng)
        position (.-startOffset rng)]
    (.insertData node position c)
    node))

(defn- new-boundary [root rng selection-type]
  (case selection-type
      :word-left
      (text/word-left root (.-startContainer rng) (.-startOffset rng))
      :word-right
      (text/word-right root (.-endContainer rng) (.-endOffset rng))
      :character-left
      (text/character-left root (.-startContainer rng) (.-startOffset rng))
      :character-right
      (text/character-right root (.-endContainer rng) (.-endOffset rng))
      :line-left
      (println "not implemented yet")
      :line-right
      (println "not implemented yet")
      :else
      (println "not implemented yet")))

(defn- extend-range [root current selection-type]
  (let [rng (get-range)]
    (when-let [new-boundary (new-boundary root rng selection-type)]
      (apply #(.setStart rng %1 %2) new-boundary)))
  current)

(defn- ->char [[c & modifiers]]
  (let [ch (char c)]
    (if (some #{:shift} modifiers)
      ch
      (.toLowerCase ch))))

(defn- handle [root current [c & modifiers :as key-desc]]
  (println "Handling key combo" key-desc)
  (cond
    (= c (.-BACKSPACE KeyCodes))
    (insert-backspace root current)
    (= c (.-ENTER KeyCodes))
    (handle-enter root current)
    (and (contains? movement-mappings key-desc))
    (move-caret root current (get movement-mappings key-desc))
    (and (contains? selection-mappings key-desc))
    (extend-range root current (get selection-mappings key-desc))
    (and (contains? modifier-mappings key-desc))
    (let [[tag-name :as tag-names] (get modifier-mappings key-desc)]
      (if-let [existing (find-tag root current tag-name)]
        (close-existing-tag root current existing)
        (open-new-tag root current tag-names)))))

(defn- modifier-map [e]
  {:alt (.-altKey e)
   :ctrl (.-ctrlKey e)
   :shift (.-shiftKey e)
   :meta (.-metaKey e)})

(defn- to-modifiers [e]
  (mapv first (filter second (modifier-map e))))

(defn- print-character [root current [c & modifiers :as key-desc]]
  (println "Printing character " key-desc)
  (when-not (some #{:meta :alt} modifiers)
    (let [new-current (insert-character root current (->char key-desc))]
      (move-caret root new-current :move-character-right)
      new-current)))

(defn- handle-keypress [root current e]
  (when-let [new-current (print-character root @current (cons (.-charCode e) (to-modifiers e)))]
    (reset! current new-current)
    (.preventDefault e)))

(defn- handle-keydown [root current e]
  (when-let [new-current
             (handle root @current (cons (.-keyCode e) (to-modifiers e)))]
    (reset! current new-current)
    (.preventDefault e)))

(defn- handle-mouseup [e]
  (println "Caret: " (.-startContainer (get-range))))

(defn- edit [root]
  (let [current (atom (append-default-structural-element root []))]
    (println "Caret: " (.-startContainer (get-range)))
    (events/listen (dom/getDocument) "keydown"
                   (partial handle-keydown root current))
    (events/listen (dom/getDocument) "keypress"
                   (partial handle-keypress root current))
    (events/listen (dom/getDocument) "mouseup"
                   (partial handle-mouseup root current))))

(edit (dom/getElement "editor"))
