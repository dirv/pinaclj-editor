(ns pinaclj-editor.core2
  (:require [clojure.zip :as zip]
            [pinaclj-editor.zipper :as zipper]
            [pinaclj-editor.key-codes :as key-codes]
            [pinaclj-editor.render :as render]))

(defn insert-character [[loc position] key-desc]
  (let [text (zip/node loc)]
    [(zip/replace loc (str (subs text 0 position)
                           (key-codes/->char key-desc)
                           (subs text position))) (inc position)]))

(defn- initial-loc []
  (-> (zipper/->zip [:div [:p "This is a test"]])
      zip/down
      zip/down))

(defn edit [root-element]
  (let [caret (atom [(initial-loc) 1])]
    (render/patch root-element (-> (first @caret) zip/root))
    (.addEventListener
      js/document
      "keypress"
      (fn [e]
        (reset! caret (insert-character @caret (key-codes/from-keypress e)))
        (render/patch root-element (-> @caret zip/root zip/node))))))
