(ns pinaclj-editor.caret
  (:require [pinaclj-editor.text-node-navigator :as nav]))

(defn- create-range []
  (.createRange js/document))

(defn- add-range [rng sel]
  (.addRange sel rng)
  rng)

(defn- get-range []
  (let [sel (.getSelection js/window)]
    (if (zero? (.-rangeCount sel))
      (-> (create-range) (add-range sel))
      (.getRangeAt sel 0))))

(defn- ->caret [rng]
  [(.-startContainer rng)
   (.-startOffset rng)
   (.-endContainer rng)
   (.-endOffset rng)])

(defn- update-caret [rng [start-container start-offset end-container end-offset :as caret]]
  (.setStart rng start-container start-offset)
  (.setEnd rng end-container end-offset)
  caret)

(defn delete-range []
  (let [rng (get-range)]
    (.deleteContents rng)
    (->caret rng)))

(defn caret-at [text-node position]
  (println "Caret at" (.-textContent text-node) position)
  [text-node position text-node position])

(defn place-caret [text-node]
  (caret-at text-node 0))

(defn selection? [[start-container start-offset end-container end-offset]]
  (not (and (= start-container end-container) (= start-offset end-offset))))

(defn- update-with-range [update-fn rng]
  (some->> rng
           (->caret)
           (update-fn)
           (update-caret rng)))

(defn do-update [update-fn]
  (update-with-range update-fn (get-range)))

(defn- find-boundary [traversal-fn find-character-fn root]
  (let [node (first (traversal-fn root))]
    [node (find-character-fn node)]))

(defn- adjust-using-position [position root caret]
  (println "Position " position)
  (cond
    (= 16 (bit-and position 16))
    caret
    (= 2 (bit-and position 2))
    (find-boundary nav/forwards-caret-traversal nav/first-character-fn root)
    (= 4 (bit-and position 4))
    (find-boundary nav/backwards-caret-traversal nav/last-character-fn root)))

(defn- keep-boundary-within [root [container _ :as caret]]
  (adjust-using-position (.compareDocumentPosition root container) root caret))

(defn keep-within [root caret]
  (concat (keep-boundary-within root (take 2 caret))
          (keep-boundary-within root (drop 2 caret))))
