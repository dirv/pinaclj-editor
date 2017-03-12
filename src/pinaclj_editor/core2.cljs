(ns pinaclj-editor.core2
  (:require [clojure.zip :as zip]
            [pinaclj-editor.zipper :as zipper]
            [pinaclj-editor.caret :as caret]
            [pinaclj-editor.dom :as dom]
            [pinaclj-editor.key-codes :as key-codes]
            [pinaclj-editor.render :as render]))

(defn insert-character [[loc position] key-desc]
  (let [text (zip/node loc)]
    [(zip/replace loc (str (subs text 0 position)
                           (key-codes/->char key-desc)
                           (subs text position))) (inc position)]))

(defn- initial-loc []
  (-> (zipper/->zip [:div [:p {:id 1} "This is a test"]])
      zip/down
      zip/down))

(defn- element-id [node-loc]
  (:id (second node-loc)))

(defn- ->dom-node [node-loc]
  (.getElementById js/document (element-id node-loc)))

(defn- find-child-matching [parent-node id]
  (some #(when (= (.-id %) id) %) (dom/children parent-node)))

(defn- ->dom-caret [[text-node-loc position]]
  (let [parent-node (-> text-node-loc zip/up zip/node ->dom-node)
        text-node (find-child-matching parent-node (element-id text-node-loc))]
    [text-node position text-node position]))

(defn- update-dom-caret [caret]
  (caret/update-caret (caret/get-range)
                      (->dom-caret caret)))

(defn edit [root-element]
  (let [caret (atom [(initial-loc) 1])]
    (render/patch root-element (-> (first @caret) zip/root))
    (.addEventListener
      js/document
      "keypress"
      (fn [e]
        (reset! caret (insert-character @caret (key-codes/from-keypress e)))
        (render/patch root-element (-> (first @caret) zip/root))
        (update-dom-caret @caret)))))
