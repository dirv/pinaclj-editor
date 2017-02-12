(ns pinaclj-editor.text-node-navigator
  (:require [pinaclj-editor.dom :as dom]))

(defn- text? [node]
  (and (dom/structural-parent node)
       (= (dom/node-type node) (.-TEXT_NODE js/window.Node))))

(defn- element? [node]
  (and node (= (.-nodeType node) (.-ELEMENT_NODE js/window.Node))))

(def not-nil? (complement nil?))

(def last-child-fn #(and (element? %) (.-lastChild %)))
(def next-child-fn #(and (element? %) (.-nextChild %)))
(def first-child-fn #(and (element? %) (.-firstChild %)))
(def previous-sibling-fn #(.-previousSibling %))
(def next-sibling-fn #(.-nextSibling %))
(def first-character-fn (fn [_] 0))
(def last-character-fn (fn [text-node] (count (.-data text-node))))

(def iterate-until-nil (comp (partial take-while not-nil?) iterate))

(defn- dfs-next-node [sibling-fn child-fn node]
  (or (child-fn node)
      (some sibling-fn (iterate-until-nil dom/parent-element node))))

(defn- dfs-traverse-nodes [sibling-fn child-fn start-node]
  (iterate-until-nil (partial dfs-next-node sibling-fn child-fn) start-node))

(defn- filter-nodes [sibling-fn child-fn choose-fn? current-node]
  (filter choose-fn? (dfs-traverse-nodes sibling-fn child-fn current-node)))

(def backwards-node-traversal
  (partial filter-nodes previous-sibling-fn last-child-fn text?))

(def forwards-node-traversal
  (partial filter-nodes next-sibling-fn first-child-fn text?))

(def all-indices #(range 0 (inc (.-length %))))
(def reverse-all-indices (comp reverse all-indices))

(defn- ->caret [node position]
  [node position])

(defn caret-traversal [index-fn traversal-fn match-fn node position]
  (drop-while #(match-fn position (second %))
              (mapcat #(map ->caret (repeat %) (index-fn (.-data %)))
                      (traversal-fn node))))

(def backwards-caret-traversal
  (partial caret-traversal reverse-all-indices backwards-node-traversal <))

(def forwards-caret-traversal
  (partial caret-traversal all-indices forwards-node-traversal >))

(defn- is-whitespace? [c]
  (= \  c)) ; todo - this is not the right test for whitespace

(defn- some-or-last [pred [x & xs]]
  (if (seq xs)
    (or (pred x) (recur pred xs))
    x))

(defn- choose-caret [skip-fn traversal-fn text-node position]
  (some-or-last #(when (skip-fn text-node position %) %)
                (traversal-fn text-node position)))

(defn- not-same? [current-node current-position new-node new-position]
  (not (and (= current-node new-node) (= current-position new-position))))

(defn- ending? [node position]
  (= position (count (.-data node))))

(defn- word-boundary? [node-a position-a [node-b position-b]]
  (and (not-same? node-a position-a node-b position-b)
       (or (is-whitespace? (get (.-data node-b) position-b))
           (not= (dom/structural-parent node-a) (dom/structural-parent node-b)))))

(defn- different-character? [current-node current-position [new-node new-position]]
  (and (not-same? current-node current-position new-node new-position)
       (or (not= (dom/structural-parent current-node) (dom/structural-parent new-node))
         (not (ending? new-node new-position)))))

(def word-left
  (partial choose-caret word-boundary? backwards-caret-traversal))

(def word-right
  (partial choose-caret word-boundary? forwards-caret-traversal))

(def character-left
  (partial choose-caret different-character? backwards-caret-traversal))

(def character-right
  (partial choose-caret different-character? forwards-caret-traversal))
