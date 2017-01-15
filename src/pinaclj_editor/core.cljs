(ns pinaclj-editor.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.events :as events]
            [pinaclj-editor.validity :as validity])
  (:import [goog.events KeyCodes]))

(enable-console-print!)

(defn- initialise [current tag]
  (let [elem (dom/createElement tag)]
    (dom/append current elem)
    elem))

(defn- insertAfter [current tag]
  (let [elem (dom/createElement tag)
        text (dom/createTextNode "")]
    (dom/insertSiblingAfter elem current)
    (dom/append elem text)
    text))

(defn- insertTree [current [tag & tags]]
  (let [elem (dom/createElement tag)]
    (dom/append current elem)
    (if (seq? tags)
      (insertTree elem tags)
      elem)))

(defn- insertTextNodeAfter [current]
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
    (conj (tags-between elem (dom/getParentElement current)) (.-tagName current))))

(def modifier-mappings
  {\b [dom/TagName.STRONG]
   \i [dom/TagName.EM]
   \c [dom/TagName.CITE]
   \h [dom/TagName.H1]  ; TODO - this should be shift+meta+1
   \o [dom/TagName.OL dom/TagName.LI]
   })

(defn- open-new-tag [top current [tag-name :as tag-names]]
  (let [insert-point (validity/find-insert-point top current tag-name)]
    (insertTree insert-point tag-names)))

; todo - this only handles the first of re-tags - should use insertTree instead
(defn- close-existing-tag [current existing]
  (let [re-tags (tags-between current existing)]
    (if (seq? re-tags)
      (insertAfter existing (first re-tags))
      (insertTextNodeAfter existing))))

(defn- empty-list-item? [current]
  (and (= dom/TagName.LI (.-tagName current))
       (= 0 (count (dom/getTextContent current)))))

(defn- close-list [list-element current]
  (dom/removeNode current)
  (insertAfter list-element dom/TagName.P))

(defn- handleEnter [current]
  (let [flow-parent (validity/find-flow-element current)
        tags-between (tags-between current flow-parent)]
    (println tags-between)
    (cond
      (= dom/TagName.OL (.-tagName flow-parent))
      (if (empty-list-item? current)
        (close-list flow-parent current)
        (insertTree flow-parent tags-between))
      :else
      (insertTree (dom/getParentElement flow-parent)
                  (cons dom/TagName.P tags-between)) ; todo - this cons is a bit nasty
      )))

(defn- handle [top current c metaKey]
  (cond
    (= c (.-ENTER KeyCodes))
    (handleEnter current)
    (and (contains? modifier-mappings (char c)) metaKey)
    (let [[tag-name :as tag-names] (get modifier-mappings (char c))]
      (if-let [existing (find-tag top current tag-name)]
        (close-existing-tag current existing)
        (open-new-tag top current tag-names)))
    (and (not metaKey))
    (do
      (dom/setTextContent current (str (dom/getTextContent current) (char c)))
      current)
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
