(ns pinaclj-editor.validity
  (:require [goog.dom :as dom]))

(def phrasing-content
  [dom/TagName.A
   dom/TagName.ABBR
   dom/TagName.CITE
   dom/TagName.CODE
   dom/TagName.EM
   dom/TagName.IMG
   dom/TagName.Q
   dom/TagName.S
   dom/TagName.SAMP
   dom/TagName.SMALL
   dom/TagName.SPAN
   dom/TagName.STRONG
   dom/TagName.SUB
   dom/TagName.SUP
   dom/TagName.VAR ])

(def breaking-elements
  [dom/TagName.ARTICLE
   dom/TagName.BLOCKQUOTE
   dom/TagName.DATA
   dom/TagName.DATALIST
   dom/TagName.DIV
   dom/TagName.DFN
   dom/TagName.FIGURE
   dom/TagName.H1
   dom/TagName.H2
   dom/TagName.H3
   dom/TagName.H4
   dom/TagName.H5
   dom/TagName.H6
   dom/TagName.HEADER
   dom/TagName.OL
   dom/TagName.P
   dom/TagName.PRE
   dom/TagName.UL])

(def flow-content
  (vec (concat phrasing-content breaking-elements)))

(def allowed-children
  {dom/TagName.P phrasing-content
   dom/TagName.LI flow-content
   dom/TagName.OL [dom/TagName.LI]
   dom/TagName.UL [dom/TagName.UL]
   dom/TagName.DIV flow-content})

(def structural-elements
  [dom/TagName.P
   dom/TagName.OL
   dom/TagName.UL])

(defn- element? [node]
  (= (.-nodeType node) (.-ELEMENT_NODE js/window.Node)))

(defn- breaking? [node]
  (when (element? node)
    (some #{(.-tagName node)} breaking-elements)))

(defn- is-valid-child? [parent-tag child-tag]
  (or
    (and (some #{parent-tag} flow-content)
         (some #{child-tag} phrasing-content))
    (some #{child-tag} (get allowed-children parent-tag))))

(defn find-insert-point [top current tag-name]
  (cond
    (is-valid-child? (.-tagName current) tag-name)
    current
    (= top current)
    nil
    :else
    (find-insert-point top (.-parentElement current) tag-name)))

; todo get rid of this and use node-path
(defn find-breaking-element [root current]
  (if (or (= root current) (breaking? current))
    current
    (find-breaking-element root (.-parentElement current))))
