(ns pinaclj-editor.text-node-navigator)

(defn- text? [node]
  (= (.-nodeType node) (.-TEXT_NODE js/window.Node)))

(defn- element? [node]
  (= (.-nodeType node) (.-ELEMENT_NODE js/window.Node)))

(defn- not-empty-text? [node]
  (and (text? node) (not (zero? (.-length node)))))

(def last-child-fn #(and (element? %) (.-lastChild %)))
(def next-child-fn #(and (element? %) (.-nextChild %)))
(def first-child-fn #(and (element? %) (.-firstChild %)))
(def previous-sibling-fn #(.-previousSibling %))
(def next-sibling-fn #(.-nextSibling %))
(def parent-element #(.-parentElement %))
(def first-character-fn (fn [_] 0))
(def last-character-fn (fn [text-node] (count (.-data text-node))))

(defn- dfs-next-node [sibling-fn child-fn node]
  (or (child-fn node)
      (some sibling-fn (iterate parent-element node))))

(defn- dfs-traverse-nodes [sibling-fn child-fn start-node]
  (drop 1 (iterate (partial dfs-next-node sibling-fn child-fn) start-node)))

(defn- choose-node [sibling-fn child-fn choose-fn? current-node]
  (some #(when (choose-fn? %) %) (dfs-traverse-nodes sibling-fn child-fn current-node)))

(def previous-text-node
  (partial choose-node previous-sibling-fn last-child-fn not-empty-text?))

(def next-text-node
  (partial choose-node next-sibling-fn first-child-fn not-empty-text?))

(defn character-left [text-node position]
  (if (= 0 position)
    (when-let [node (previous-text-node text-node)]
      [node (dec (.-length node))])
    [text-node (dec position)]))

(defn character-right [text-node position]
  (if (= position (.-length text-node))
    (when-let [node (next-text-node text-node)]
      [node 1])
    [text-node (inc position)]))

; todoo... urgh..change this
(defn- find-next-whitespace [start increment text]
  (let [position (+ start increment)]
    (cond
    (or (> 0 position)  (<= (count text) position))
    nil
    (= \  (nth text position))  ; todo - this is not the right test for whitespace
    position
    :else
    (find-next-whitespace position increment text))))

; todo - change this too
(defn- jump-word [move-fn increment text-node position]
  (if-let [ws (find-next-whitespace position increment (.-data text-node))]
    [text-node ws]
    (if-let [node (move-fn text-node)]
      (jump-word move-fn increment node (.-length node))
      text-node))) ; note - if there's a null comes back, return the last node so that we never return nil

(def word-left
  (partial jump-word previous-text-node -1))

(def word-right
  (partial jump-word next-text-node 1))
