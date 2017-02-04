(ns pinaclj-editor.caret)

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
  [text-node position text-node position])

(defn place-caret [text-node]
  (caret-at text-node 0))

(defn selection? [[start-container start-offset end-container end-offset]]
  (not (and (= start-container end-container) (= start-offset end-offset))))

(defn do-update [update-fn]
  (let [rng (get-range)]
    (some->> (->caret rng)
             (update-fn)
             (update-caret rng))))
