(ns pinaclj-editor.zipper
  (:require [clojure.zip :as zip]))

(defn- children [[_ & [attrs-or-first-child remaining-children :as all-children]]]
  (if (map? attrs-or-first-child)
    remaining-children
    all-children))

(defn- make-node [element children]
  (into element children))

(def ->zip
  (partial zip/zipper vector? children make-node))
