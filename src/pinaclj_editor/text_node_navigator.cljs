(ns pinaclj-editor.text-node-navigator)

(defn- text? [node]
  (= (.-nodeType (.-TEXT_NODE js/window.Node))))

(def last-child-fn #(.-lastChild %))
(def next-child-fn #(.-nextChild %))
(def previous-sibling-fn #(.-previousSibling %))
(def next-sibling-fn #(.-nextSibling %))

(defn- find-deepest-child [child-fn node]
  (if-let [child (child-fn node)]
    (find-deepest-child child child-fn)
    node))

(defn- node-seq [sibling-fn child-fn root current-node]
  (if (= root current-node)
    []
    (let [sibling (sibling-fn current-node)]
      (if (= nil sibling)
        (node-seq sibling-fn child-fn root (.-parentElement current-node))
        (let [deepest-node (find-deepest-child child-fn sibling)]
          (cons deepest-node (node-seq sibling-fn child-fn root deepest-node)))))))

(defn- choose-node [sibling-fn child-fn choose-fn? root current-node]
  (let [nodes (filter choose-fn? (node-seq sibling-fn child-fn root current-node))]
    (when-not (empty? nodes)
      (first nodes))))

; todo - does not handle empty text-nodes
(def previous-text-node
  (partial choose-node previous-sibling-fn last-child-fn text?))

; todo - does not handle empty text-nodes
(def next-text-node
  (partial choose-node next-sibling-fn next-child-fn text?))

(defn character-left [root text-node position]
  (if (= 0 position)
    (when-let [node (previous-text-node root text-node)]
      [node (dec (.-length node))])
    [text-node (dec position)]))

(defn character-right [root text-node position]
  (if (= position (dec (.-length text-node)))
    (when-let [node (next-text-node root text-node)]
      [node 0])
    [text-node (inc position)]))

(defn- find-next-whitespace [start increment text]
  (cond
    (or (> 0 start)  (< (count text) start))
    nil
    (= \  (nth text start))  ; todo - this is not the right test for whitespace
    start
    :else
    (find-next-whitespace (+ start increment) increment text)))

(defn- jump-word [move-fn increment root text-node position]
  (if-let [ws (find-next-whitespace position increment (.-data text-node))]
    [text-node position]
    (when-let [node (move-fn root text-node)]
      (jump-word move-fn increment root node (.-length node)))))

(def word-left
  (partial jump-word previous-text-node -1))

(def word-right
  (partial jump-word next-text-node 1))
