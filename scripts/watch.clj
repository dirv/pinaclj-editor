(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'pinaclj-editor.core
   :output-to "out/pinaclj_editor.js"
   :output-dir "out"})
