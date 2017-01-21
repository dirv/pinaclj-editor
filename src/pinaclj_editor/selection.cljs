(ns pinaclj-editor.selection)

(defn- start-boundary [rng]
  [(.-startContainer rng) (.-startOffset rng)])

(defn- set-start-boundary [rng [node offset]]
  (.setStart rng node offset))

(defn- set-end-boundary [rng [node offset]]
  (.setEnd rng node offset))

(defn change [rng current-boundary new-boundary]
  (if (= (start-boundary rng) current-boundary)
    (set-start-boundary rng new-boundary)
    (set-end-boundary rng new-boundary)))
