(ns pinaclj-editor.zipper
  (:require [clojure.zip :as zip]))

(defn- children [[_ & [attrs-or-first-child & remaining-children :as all-children]]]
  (if (map? attrs-or-first-child)
    remaining-children
    all-children))

(defn- make-node [[tag & attrs-and-children] children]
  (if (map? (first attrs-and-children))
    (into [tag (first attrs-and-children)] children)
    (into [tag] children)))

(def ->zip
  (partial zip/zipper vector? children make-node))

(defn- root-loc [z]
  (last (take-while #(not (nil? %)) (iterate zip/up z))))
