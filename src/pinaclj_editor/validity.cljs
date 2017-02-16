(ns pinaclj-editor.validity
  (:require [pinaclj-editor.dom :as pdom]))

(def phrasing-content
  [:A
   :ABBR
   :CITE
   :CODE
   :EM
   :IMG
   :Q
   :S
   :SAMP
   :SMALL
   :SPAN
   :STRONG
   :SUB
   :SUP
   :VAR ])

(def breaking-elements
  [:ARTICLE
   :BLOCKQUOTE
   :DATA
   :DATALIST
   :DIV
   :DFN
   :FIGURE
   :H1
   :H2
   :H3
   :H4
   :H5
   :H6
   :HEADER
   :OL
   :P
   :PRE
   :UL])

(def flow-content
  (vec (concat phrasing-content breaking-elements)))

(def allowed-children
  {nil phrasing-content
   :P phrasing-content
   :LI flow-content
   :OL [:LI]
   :UL [:UL]
   :DIV flow-content})

(def structural-elements
  [:P
   :OL
   :UL])

(defn- element? [node]
  (= (.-nodeType node) (.-ELEMENT_NODE js/window.Node)))

(defn- breaking? [node]
  (and (element? node)
    (some #{(pdom/tag node)} breaking-elements)))

(defn- is-valid-child? [parent-tag child-tag]
  (or
    (and (some #{parent-tag} flow-content)
         (some #{child-tag} phrasing-content))
    (some #{child-tag} (get allowed-children parent-tag))))

(defn find-insert-point [current tag-name]
  (some #(when (is-valid-child? (pdom/tag %) tag-name) %) (pdom/node-path current)))

(defn find-breaking-element [current]
 (some #(when (breaking? %) %) (pdom/node-path current)))
