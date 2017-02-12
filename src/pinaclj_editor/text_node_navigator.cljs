(ns pinaclj-editor.text-node-navigator)

(defn- text? [node]
  (and node (= (.-nodeType node) (.-TEXT_NODE js/window.Node))))

(defn- element? [node]
  (and node (= (.-nodeType node) (.-ELEMENT_NODE js/window.Node))))
(defn- not-empty-text? [node] (and (text? node) (not (zero? (.-length node)))))

(def not-nil? (complement nil?))

(def last-child-fn #(and (element? %) (.-lastChild %)))
(def next-child-fn #(and (element? %) (.-nextChild %)))
(def first-child-fn #(and (element? %) (.-firstChild %)))
(def previous-sibling-fn #(.-previousSibling %))
(def next-sibling-fn #(.-nextSibling %))
(def parent-element #(.-parentElement %))
(def first-character-fn (fn [_] 0))
(def last-character-fn (fn [text-node] (count (.-data text-node))))

(def iterate-until-nil (comp (partial take-while not-nil?) iterate))

(defn- dfs-next-node [sibling-fn child-fn node]
  (or (child-fn node)
      (some sibling-fn (iterate-until-nil parent-element node))))

(defn- dfs-traverse-nodes [sibling-fn child-fn start-node]
  (iterate-until-nil (partial dfs-next-node sibling-fn child-fn) start-node))

(defn- filter-nodes [sibling-fn child-fn choose-fn? current-node]
  (filter choose-fn? (dfs-traverse-nodes sibling-fn child-fn current-node)))

(def backwards-traversal
  (partial filter-nodes previous-sibling-fn last-child-fn not-empty-text?))

(def forwards-traversal
  (partial filter-nodes next-sibling-fn first-child-fn not-empty-text?))

(defn next-text-node [node]
  (first (drop 1 (forwards-traversal node))))

(defn character-left [text-node position]
  (if (= 0 position)
    (when-let [node (first (drop 1 (backwards-traversal text-node)))]
      [node (dec (.-length node))])
    [text-node (dec position)]))

(defn character-right [text-node position]
  (if (= position (.-length text-node))
    (when-let [node (first (drop 1 (forwards-traversal text-node)))]
      [node 1])
    [text-node (inc position)]))

(defn- is-whitespace? [index c]
  (when (= \  c) index)) ; todo - this is not the right test for whitespace

(def whitespace-indexes (partial keep-indexed is-whitespace?))

(def reverse-whitespace-indexes (comp reverse whitespace-indexes) )

(defn- whitespace-list [whitespace-fn traversal-fn text-node]
  (mapcat #(map vector (repeat %) (whitespace-fn (.-data %)))
          (traversal-fn text-node)))

(defn- before-this? [text-node position [new-node new-position]]
  (or (not= text-node new-node)
      (< new-position position)))

(defn- after-this? [text-node position [new-node new-position]]
  (or (not= text-node new-node)
      (> new-position position)))

(defn- choose-next-word [skip-fn whitespace-fn traversal-fn text-node position]
  (some #(when (skip-fn text-node position %) %)
        (whitespace-list whitespace-fn traversal-fn text-node)))

(def word-left
  (partial choose-next-word before-this? reverse-whitespace-indexes backwards-traversal))

(def word-right
  (partial choose-next-word after-this? whitespace-indexes forwards-traversal))
