(ns pinaclj-editor.render
  (:require [incremental-dom :as idom]))

(defn- select-type [node]
  (if (vector? node)
    (if (or (= 1 (count node))
            (and (= 2 (count node)) (map? (second node))))
      :leaf-element
      :parent-element)
    :text-node))

(defn- open-element [tag id attrs]
  (apply idom/elementOpen (name tag) id []
         (mapcat #(list (name (key %)) (val %)) attrs)))

(defmulti render select-type)
(defmethod render :leaf-element
  [[tag attrs]]
  (apply idom/void tag nil [] attrs))

(defmethod render :parent-element
  [[tag & [attrs-or-first-child remaining-children :as all-children]]]
  (if (map? attrs-or-first-child)
    (do (open-element tag nil attrs-or-first-child)
        (doall (map render remaining-children)))
    (do (open-element tag nil [])
        (doall (map render all-children))))
  (idom/elementClose (name tag)))

(defmethod render :text-node [text] (idom/text text))

(defn patch [element data]
  (idom/patch element #(render %) data))

