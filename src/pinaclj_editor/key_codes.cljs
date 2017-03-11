(ns pinaclj-editor.key-codes)

(def character-mappings
  (into {} (map #(vector % (keyword (str (char %)))) (range 49 91))))

(def mappings
  (merge
    character-mappings
    {8 :backspace
     13 :enter
     37 :left
     38 :up
     39 :right
     40 :down
     }))

(defn- modifier-map [key-event]
  {:alt (.-altKey key-event)
   :ctrl (.-ctrlKey key-event)
   :shift (.-shiftKey key-event)
   :meta (.-metaKey key-event)})

(defn- key-code [key-event]
  (get mappings (.-keyCode key-event)))

(defn- modifiers-of [key-event]
  (mapv first (filter second (modifier-map key-event))))

(defn from-keydown [e]
  (cons (key-code e) (modifiers-of e)))

(defn from-keypress [e]
  (cons (.-charCode e) (modifiers-of e)))

(defn ->char [[c & modifiers]]
  (let [ch (char c)]
    (if (some #{:shift} modifiers)
      ch
      (.toLowerCase ch))))
