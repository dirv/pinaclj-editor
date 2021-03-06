(ns pinaclj-editor.core
  (:require [clojure.browser.repl :as repl]
            [pinaclj-editor.core2 :as core2]
            [pinaclj-editor.render :as render]
            [pinaclj-editor.dom :as pdom]
            [pinaclj-editor.caret :as caret]
            [pinaclj-editor.command-bindings :as bindings]
            [pinaclj-editor.key-codes :as key-codes]
            [pinaclj-editor.text-node-navigator :as text]
            [pinaclj-editor.validity :as validity]))

(enable-console-print!)

(defn- move-caret [root [start-container start-offset end-container end-offset] movement-type]
  (caret/keep-within
    root
    (apply caret/caret-at
           (case movement-type
             :move-character-left
             (text/character-left start-container start-offset)
             :move-character-right
             (text/character-right end-container end-offset)
             :move-word-left
             (text/word-left start-container start-offset)
             :move-word-right
             (text/word-right end-container end-offset)))))

(defn- open-new-tag [[text-node position :as caret] [tag-name :as tag-names]]
  (let [insert-point (validity/find-insert-point text-node tag-name)
        new-text-node (pdom/split-tree insert-point caret)]
    (pdom/remove-empty-nodes new-text-node)
    (pdom/insert-tree-after insert-point (concat tag-names [""]))))

(defn- close-existing-tag [[node _ :as caret] existing]
  (let [tags-between (pdom/tags-between-inclusive node existing)
        new-tag (pdom/split-tree-with-tags existing caret tags-between)]
    (pdom/insert-tree-after existing (drop 1 tags-between))))

(def default-structural-nodes [:P ""])

(defn- close-list [list-element li-element re-tags]
  (pdom/remove-node li-element)
  (let [text-node (pdom/insert-tree-before (.-parentElement list-element)
                                           (.-nextSibling list-element)
                                           (concat re-tags default-structural-nodes))]
    (if (= 0 (count (.-children list-element)))
      (pdom/remove-node list-element))
    (caret/place-caret text-node)))

(defn- break-at-enter [breaker caret]
  (caret/place-caret (pdom/split-tree breaker caret)))

(defn- handle-enter [_]
  (let [caret (caret/delete-range) ; todo- is this the right place for it?
        text-node (nth caret 2)
        breaker (validity/find-breaking-element text-node)]
    (cond
      (= :OL (pdom/tag breaker))
      (let [li-element (pdom/find-tag text-node :LI)]
        (if (pdom/node-empty? text-node)
          (close-list breaker li-element (pdom/tags-between text-node li-element))
          (break-at-enter breaker caret)))
      :else
      (break-at-enter breaker caret))))

; todo - this could be used for the delete key too...
(defn- delete-and-merge [root [current-node current-position :as caret]]
  (when-let [[new-node new-position :as new-caret] (move-caret root caret :move-character-left)]
    (when (not= new-caret caret)
      (let [structural-parents (map pdom/structural-parent [current-node new-node])]
        (if (apply not= structural-parents)
          (apply pdom/merge-nodes structural-parents)
          (pdom/delete-single-character new-caret))))
    new-caret))

(defn- insert-backspace [root [start-container start-offset end-container end-offset :as caret]]
  (if (caret/selection? caret)
    (caret/delete-range)  ; todo - need to ensure that the range is within our own document
    (delete-and-merge root caret)))

(defn- insert-character [[start-container start-offset end-container end-offset] c]
  (.insertData end-container end-offset c)
  (caret/caret-at end-container (inc end-offset)))

; todo - needs to change to take into account left or right side of caret.
(defn- extend-range [[start-container start-offset end-container end-offset] selection-type]
  (case selection-type
      :word-left
      (concat (text/word-left start-container start-offset) [end-container end-offset])
      :word-right
      (concat [start-container start-offset] (text/word-right end-container end-offset))
      :character-left
      (concat (text/character-left start-container start-offset) [end-container end-offset])
      :character-right
      (concat [start-container start-offset] (text/character-right end-container end-offset))
      :line-left
      (println "not implemented yet")
      :line-right
      (println "not implemented yet")
      :else
      (println "not implemented yet")))

; todo - this should work on selections too
(defn- decorate-nodes [tags caret]
  (when-not (caret/selection? caret)
    (caret/place-caret
      (let [element (first caret)]
        (if-let [existing (pdom/find-tag element (first tags))]
          (close-existing-tag caret existing)
          (open-new-tag caret tags))))))

; todo - delete selection first
(defn- perform-action [root [c & modifiers :as key-desc] caret]
  (println "Handling key combo" key-desc)
  (cond
    (= c :backspace)
    (insert-backspace root caret)
    (= c :enter)
    (handle-enter caret)
    (and (contains? bindings/movement-mappings key-desc))
    (move-caret root caret (get bindings/movement-mappings key-desc))
    (and (contains? bindings/selection-mappings key-desc))
    (extend-range caret (get bindings/selection-mappings key-desc))
    (and (contains? bindings/modifier-mappings key-desc))
    (decorate-nodes (get bindings/modifier-mappings key-desc) caret)))

(defn- print-character [[c & modifiers :as key-desc] _]
  (when-not (some #{:meta :alt} modifiers)
    (let [caret (caret/delete-range)]
      (insert-character caret (key-codes/->char key-desc)))))

(defn- handle-keypress [root e]
  (when (caret/do-update (partial print-character (key-codes/from-keypress e)))
    (.preventDefault e)))

(defn- handle-keydown [root e]
  (when (caret/do-update (partial perform-action root (key-codes/from-keydown e)))
    (.preventDefault e)))

(defn- handle-mouseup [root e]
  (caret/do-update (partial println)))

(defn- initialize-document [root _]
  (apply caret/caret-at
    (or (first (text/forwards-caret-traversal root 0))
        [(pdom/append-tree root default-structural-nodes) 0])))

(defn- edit [root]
  (caret/do-update (partial initialize-document root))
  (.addEventListener js/document "keydown" (partial handle-keydown root))
  (.addEventListener js/document "keypress" (partial handle-keypress root))
  (.addEventListener js/document "mouseup" (partial handle-mouseup root)))

;(edit (.getElementById js/document "editor"))
(core2/edit (.getElementById js/document "editor"))
