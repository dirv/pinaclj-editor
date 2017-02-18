(ns pinaclj-editor.command-bindings)

(def modifier-mappings
  {[:B :meta] [:STRONG]
   [:I :meta] [:EM]
   [:C :meta] [:CITE]
   [:1 :shift :meta] [:H1]
   [:2 :shift :meta] [:H2]
   [:3 :shift :meta] [:H3]
   [:4 :shift :meta] [:H4]
   [:5 :shift :meta] [:H5]
   [:6 :shift :meta] [:H6]
   [:O :meta] [:OL :LI]
   })

(def selection-mappings
  {[:left :shift] :character-left
   [:right :shift] :character-right
   [:left :alt :shift] :word-left
   [:right :alt :shift] :word-right
   [:left :shift :meta] :line-left
   [:right :shift :meta] :line-right})

(def movement-mappings
  {[:left] :move-character-left
   [:right] :move-character-right
   [:left :alt] :move-word-left
   [:right :alt] :move-word-right
   [:left :meta] :move-line-left
   [:right :meta] :move-line-right})

