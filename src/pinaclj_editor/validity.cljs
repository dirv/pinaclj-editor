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

(def flow-content
  (vec (concat
    phrasing-content
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
     dom/TagName.UL])))

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

(defn- structural? [element]
  (some #{(.-tagName element)} structural-elements))

(defn- is-valid-child? [parent child-tag]
  (let [parent-tag (.-tagName parent)]
    (or
      (and (some #{parent-tag} flow-content)
           (some #{child-tag} phrasing-content))
      (some #{child-tag} (get allowed-children parent-tag)))))

(defn find-insert-point [top current tag-name]
  (cond
    (is-valid-child? current tag-name)
    current
    (= top current)
    nil
    :else
    (find-insert-point top (dom/getParentElement current) tag-name)))

; todo get rid of this and use node-path
(defn find-structural-element [current]
  (if (structural? current) 
    current
    (find-structural-element (dom/getParentElement current))))
