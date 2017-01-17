(ns pinaclj-editor.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]
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

(defn- find-tag [top current tag-name]
  (cond
    (= top current)
    nil
    (= (.-tagName current) tag-name)
    current
    :else
    (find-tag top (dom/getParentElement current) tag-name)))

(defn- tags-between [current elem]
  (if (= (.-tagName current) (.-tagName elem))
    []
    (conj (tags-between (dom/getParentElement current) elem) (.-tagName current))))

(def modifier-mappings
  {\b [dom/TagName.STRONG]
   \i [dom/TagName.EM]
   \c [dom/TagName.CITE]
   \h [dom/TagName.H1]  ; TODO - this should be shift+meta+1
   \o [dom/TagName.OL dom/TagName.LI]
   })

(defn- open-new-tag [top current [tag-name :as tag-names]]
  (let [insert-point (validity/find-insert-point top current tag-name)]
    (insert-tree insert-point tag-names)))

(defn- close-existing-tag [top current existing]
  (let [re-tags (tags-between current existing)]
    (if (empty? re-tags)
      (insert-text-node-after existing)    ; todo - is tihs right?
      (insert-tree (validity/find-insert-point top (dom/getParentElement existing) (first re-tags)) re-tags))))

(defn- empty-list-item? [current]
  (= 0 (count (dom/getTextContent current))))

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
        (if (empty-list-item? current)
          (close-list parent li-element)
          (insert-tree parent tags-between)))
      :else
      (insert-tree (dom/getParentElement parent)
                  (cons dom/TagName.P tags-between)) ; todo - this cons is a bit nasty
      )))

(defn- node-path [top node]
  (if (= top node)
    []
    (cons node (node-path top (dom/getParentElement node)))))

(defn- is-empty [node]
  (= 0 (count (dom/getTextContent node))))

(defn- all-empty [top current]
  (take-while is-empty (node-path top current)))

(defn- one-fewer-characters [current]
  (let [content (dom/getTextContent current)]
    (subs content 0 (max 0 (dec (count content))))))

(defn- insert-backspace [top current]
  (dom/setTextContent current (one-fewer-characters current))
  (when-let [empty-tree (seq (all-empty top current))]
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
(defn- insert-character [top current c]
  (if (= top current)
    (let [p (dom/createElement dom/TagName.P)
          text (dom/createTextNode c)]
      (dom/appendChild p text)
      (dom/appendChild current p)
      p)
    (append-character-to-node current c)))

(defn- handle [top current c metaKey]
  (println c)
  (cond
    (= c (.-BACKSPACE KeyCodes))
    (insert-backspace top current)
    (= c (.-ENTER KeyCodes))
    (handle-enter current)
    (and (contains? modifier-mappings (char c)) metaKey)
    (let [[tag-name :as tag-names] (get modifier-mappings (char c))]
      (if-let [existing (find-tag top current tag-name)]
        (close-existing-tag top current existing)
        (open-new-tag top current tag-names)))
    (and (not metaKey))
    (insert-character top current (char c))
    :else
    nil))


(defn- handle-keypress-event [top current e]
  (when-let [new-current (handle top @current (.-charCode e) (.-metaKey e))]
    (reset! current new-current)
    (.preventDefault e)))

(defn- edit [top current]
  (let [current (atom current)]
    (events/listen (dom/getDocument) "keypress"
                   (partial handle-keypress-event top current))))

(let [editor (dom/getElement "editor")
      p (dom/createElement dom/TagName.P)
      node (dom/createTextNode "")]
  (dom/appendChild editor p)
  (dom/appendChild p node)
  (edit editor node))
