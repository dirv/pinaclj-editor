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

; todo - what about text nodes (with no tag name)
(defn- insert-tree [current [tag & tags]]
  (let [elem (dom/createElement tag)]
    (dom/append current elem)
    (if (seq? tags)
      (insert-tree elem tags)
      elem)))

(defn- insert-text-node-after [current]
  (let [node (dom/createTextNode "")]
    (dom/insertSiblingAfter node current)
    node))

(defn- find-tag [root current tag-name]
  (cond
    (= root current)
    nil
    (= (.-tagName current) tag-name)
    current
    :else
    (find-tag root (dom/getParentElement current) tag-name)))

(defn- tags-between [current elem]
  (if (= (.-tagName current) (.-tagName elem))
    []
    (conj (tags-between (dom/getParentElement current) elem) (.-tagName current))))

(def modifier-mappings
  {[(.-B KeyCodes) :meta] [dom/TagName.STRONG]
   [\i :meta] [dom/TagName.EM]
   [\c :meta] [dom/TagName.CITE]
   [33 :shift :meta] [dom/TagName.H1]
   [\2 :shift :meta] [dom/TagName.H2]
   [\3 :shift :meta] [dom/TagName.H3]
   [\4 :shift :meta] [dom/TagName.H4]
   [\5 :shift :meta] [dom/TagName.H5]
   [\6 :shift :meta] [dom/TagName.H6]
   [\o :meta] [dom/TagName.OL dom/TagName.LI]
   })

(def selection-mappings
  {[(.-LEFT KeyCodes) :shift] :character-left
   [(.-RIGHT KeyCodes) :shift] :character-right
   [(.-LEFT KeyCodes) :alt :shift] :word-left
   [(.-RIGHT KeyCodes) :alt :shift] :word-right
   [(.-LEFT KeyCodes) :shift :meta] :line-left
   [(.-RIGHT KeyCodes) :shift :meta] :line-right})

(defn- open-new-tag [root current [tag-name :as tag-names]]
  (let [insert-point (validity/find-insert-point root current tag-name)]
    (insert-tree insert-point tag-names)))

(defn- close-existing-tag [root current existing]
  (let [re-tags (tags-between current existing)]
    (if (empty? re-tags)
      (insert-text-node-after existing)    ; todo - is tihs right?
      (insert-tree (validity/find-insert-point root (dom/getParentElement existing) (first re-tags)) re-tags))))

(defn- node-empty? [node]
  (= 0 (count (dom/getTextContent node))))

(defn- close-list [list-element current]
  (dom/removeNode current)
  (let [new-parent (insert-after list-element dom/TagName.P)]
    (if (= 0 (.-length (dom/getChildren list-element)))
      (dom/removeNode list-element))
    new-parent))

(defn- handle-enter [current]
  (let [parent (validity/find-structural-element current)
        tags-between (tags-between current parent)]
    (cond
      (= dom/TagName.OL (.-tagName parent))
      (let [li-element (find-tag parent current dom/TagName.LI)]
        (if (node-empty? current)
          (close-list parent li-element)
          (insert-tree parent tags-between)))
      :else
      (insert-tree (dom/getParentElement parent)
                  (cons dom/TagName.P tags-between)) ; todo - this cons is a bit nasty
      )))

; todo; is this better done with node-seq in text node navigator?
(defn- node-path [root node]
  (if (= root node)
    []
    (cons node (node-path root (dom/getParentElement node)))))

(defn- all-empty [root current]
  (take-while node-empty? (node-path root current)))

(defn- one-fewer-characters [current]
  (let [content (dom/getTextContent current)]
    (subs content 0 (max 0 (dec (count content))))))

(defn- insert-backspace [root current]
  (dom/setTextContent current (one-fewer-characters current))
  (when-let [empty-tree (seq (all-empty root current))]
    (let [to-remove (last empty-tree)
          parent (dom/getParentElement to-remove)]
      (dom/removeNode to-remove)
      parent)))

(defn- append-character-to-node [current c]
  (dom/setTextContent
    current
    (str (dom/getTextContent current) c))
  current)

; todo - extract out the p node creation with a text node
(defn- insert-character [root current c]
  (if (= root current)
    (let [p (dom/createElement dom/TagName.P)
          text (dom/createTextNode c)]
      (dom/appendChild p text)
      (dom/appendChild current p)
      p)
    (append-character-to-node current c)))

(defn- get-range [n]
  (.getRangeAt (.getSelection js/window) n))

(defn- new-boundary [root rng selection-type]
  (case selection-type
      :word-left
      (text/word-left root (.-startContainer rng) (.-startOffset rng))
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
  (let [rng (get-range 0)]
    (when-let [new-boundary (new-boundary root rng selection-type)]
    (apply #(.setStart rng %1 %2) new-boundary)))
  current) ; todo - possibly not return current?

(defn- create-range [current]
  (println "Creating range")
  (let [r (.createRange js/document)
        selection (.getSelection js/window)]
    (.setStart r (.-firstChild current) 0)
    (.collapse r true)
    (.addRange selection r)))

(defn- create-range-if-necessary [current]
  (when (= 0 (.-rangeCount (.getSelection js/window)))
    (create-range current)))

; todo - this should take into account text nodes, but it doesn't
; there is still the outstanding question of if 'current' should be text nodes or
; the parent.
(defn- move-caret [current]
  (println "Moving caret")
  (create-range-if-necessary current)
  (let [r (get-range 0)]
    (println "Moving to " (count (dom/getTextContent current)))
    (.setStart r (.-firstChild current) (count (dom/getTextContent current)))))

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
    (handle-enter current)
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
      (move-caret new-current)
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

(defn- edit [root]
  (let [current (atom root)]
    (events/listen (dom/getDocument) "keydown"
                   (partial handle-keydown root current))
    (events/listen (dom/getDocument) "keypress"
                   (partial handle-keypress root current))))

(edit (dom/getElement "editor"))
