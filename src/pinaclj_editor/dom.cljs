(ns pinaclj-editor.dom)

(defmulti create-element identity)
(defmethod create-element "" [_] (.createTextNode js/document ""))
(defmethod create-element nil [_] (.createTextNode js/document ""))
(defmethod create-element :default [tag] (.createElement js/document tag))

(defn- remove-node [node]
  (.removeChild (.-parentElement node) node))

(defn- insert-after [before element]
  (.insertBefore (.-parentElement before) element (.-nextSibling before))
  element)

(defn- insert-before [element parent sibling]
  (.insertBefore parent element sibling))

(defn- ->elements [tags]
  (map create-element tags))

(defn- append-child [parent child]
  (.appendChild parent child)
  child)

(defn reparent [new-parent node]
  (println "Reparenting " (.-textContent node))
  (append-child new-parent (remove-node node)))

(defn node-path [node]
  (take-while some? (iterate #(.-parentElement %) node)))

(defn- node-empty? [node]
  (= 0 (count (.-textContent node))))

(defn- all-empty-nodes [current]
  (take-while node-empty? (node-path current)))

(defn remove-empty-nodes [node]
  (map #(.removeNode %) (all-empty-nodes node)))

(defn- root [element]
  (last (node-path element)))

(defn create-tree [tags]
  (reduce append-child (->elements tags)))

(defn append-tree [parent tags]
  (let [deepest-child (create-tree tags)]
    (append-child parent (root deepest-child))
    deepest-child))

(defn insert-tree-before [parent sibling tags]
  (let [deepest-child (create-tree tags)]
    (insert-before (root deepest-child) parent sibling)
    deepest-child))

(defn insert-tree-after [sibling tags]
  (let [deepest-child (create-tree tags)]
    (insert-after sibling (root deepest-child))
    deepest-child))

(defn find-tag [node tag]
  (some #(when (= (.-tagName %) tag) %) (node-path node)))

(defn nodes-between [child stop-node]
  (take-while (partial not= stop-node) (node-path child)))

(defn nodes-between-inclusive [child stop-node]
  (concat (nodes-between child stop-node) (list stop-node)))

(defn tags-between [child stop-node]
  (reverse (map #(.-tagName %) (nodes-between child stop-node))))

(defn next-siblings [element]
  (drop 1 (take-while some? (iterate #(.-nextSibling %) element))))

(defn child-containing-node [parent search-node]
  (some #(when (.contains % search-node) %) (array-seq (.-childNodes parent))))

(defn all-siblings-after [parent search-node]
  (next-siblings (child-containing-node parent search-node)))
