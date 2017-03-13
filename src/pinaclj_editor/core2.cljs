(ns pinaclj-editor.core2
  (:require [clojure.zip :as zip]
            [pinaclj-editor.zipper :as zipper]
            [pinaclj-editor.caret :as caret]
            [pinaclj-editor.command-bindings :as bindings]
            [pinaclj-editor.dom :as dom]
            [pinaclj-editor.key-codes :as key-codes]
            [pinaclj-editor.render :as render]))

(def next-id (atom 2))

(defn- gen-id []
  (let [id @next-id]
    (swap! next-id inc)
    id))

(defn insert-character [[loc position] key-desc]
  (let [text (zip/node loc)]
    [(zip/replace loc (str (subs text 0 position)
                           (key-codes/->char key-desc)
                           (subs text position))) (inc position)]))

(defn- initial-loc []
  (-> (zipper/->zip [:div {:id "root"} [:p {:id (gen-id)} "This is a test"]])
      zip/down
      zip/down))

(defn- element-id [node-loc]
  (:id (second (zip/node node-loc))))

(defn- ->dom-node [node-loc]
  (.getElementById js/document (element-id node-loc)))

(defn- find-child-matching [parent-node id]
  (some #(when (= (.-id %) id) %) (dom/children parent-node)))

(defn- ->dom-caret [[text-node-loc position]]
  (let [parent-node (-> text-node-loc zip/up ->dom-node)
        text-node (find-child-matching parent-node (element-id text-node-loc))]
    [text-node position text-node position]))

(defn- update-dom-caret [caret]
  (caret/update-caret (caret/get-range)
                      (->dom-caret caret)))

(defn- adjust-to-caret [loc text-node]
  (if (= (element-id loc) (.-id text-node))
    loc
    (if (.contains (->dom-node loc) text-node)
      (adjust-to-caret (zip/down loc) text-node)
      (adjust-to-caret (zip/right loc) text-node))))

; todo - rebuild any tree of nodes
(defn- decorate-nodes [[text-node-loc position] key-desc]
  (when (contains? bindings/modifier-mappings key-desc)
    (let [text (zip/node text-node-loc)]
      [(-> text-node-loc
        (zip/replace (subs text 0 position))
        (zip/insert-right (subs text position))
        (zip/insert-right [:b {:id (gen-id)} ""])
        (zip/right)
        (zip/down)) 0])))

(defn edit [root-element]
  (let [caret (atom [(initial-loc) 1])]
    (render/patch root-element (-> (first @caret) zip/root))
    (.addEventListener
      js/document
      "keydown"
      (fn [e]
        (when-let [new-caret (decorate-nodes @caret (key-codes/from-keydown e))]
          (reset! caret new-caret)
          (render/patch root-element (-> (first @caret) zip/root))
          (update-dom-caret @caret)
          (.preventDefault e))))
    (.addEventListener
      js/document
      "keypress"
      (fn [e]
        (reset! caret (insert-character @caret (key-codes/from-keypress e)))
        (render/patch root-element (-> (first @caret) zip/root))
        (update-dom-caret @caret)))
    (.addEventListener
      js/document
      "mouseup"
      (fn [e]
        (let [rng (caret/get-range)]
          (reset! caret
                  [(adjust-to-caret (zipper/root-loc (first @caret))
                                   (.-startContainer rng))
                   (.-startOffset rng)]))
        ))))
