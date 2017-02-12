(ns pinaclj-editor.text-node-navigator
  (:require [pinaclj-editor.dom :as dom]))

(defn- text? [node]
  (and (dom/is-one-of? (dom/parent-element node) dom/structural-elements)
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

(def backwards-traversal
  (partial filter-nodes previous-sibling-fn last-child-fn text?))

(def forwards-traversal
  (partial filter-nodes next-sibling-fn first-child-fn text?))

(defn- is-whitespace? [index c]
  (when (= \  c) index)) ; todo - this is not the right test for whitespace

(def whitespace-indices (partial keep-indexed is-whitespace?))

(def reverse-whitespace-indices (comp reverse whitespace-indices))

(def all-indices #(range 0 (inc (.-length %))))
(def reverse-all-indices (comp reverse all-indices))

(defn- matching-characters [character-match-fn traversal-fn text-node]
  (mapcat #(map vector (repeat %) (character-match-fn (.-data %)))
          (traversal-fn text-node)))

(defn- before-this? [text-node position [new-node new-position]]
  (or (not= text-node new-node)
      (< new-position position)))

(defn- after-this? [text-node position [new-node new-position]]
  (or (not= text-node new-node)
      (> new-position position)))

(defn- choose-caret [skip-fn character-match-fn traversal-fn text-node position]
  (or (some #(when (skip-fn text-node position %) %)
            (matching-characters character-match-fn traversal-fn text-node))
      [text-node position]))

(def word-left
  (partial choose-caret before-this? reverse-whitespace-indices backwards-traversal))

(def word-right
  (partial choose-caret after-this? whitespace-indices forwards-traversal))

(def character-left
  (partial choose-caret before-this? reverse-all-indices backwards-traversal))

(def character-right
  (partial choose-caret after-this? all-indices forwards-traversal))
