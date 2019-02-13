(ns expert-system.core)

(load "lexer")
(load "parser")

(defn open-file-lines [fileName]
  (with-open [rdr (clojure.java.io/reader fileName)]
    (reduce conj [] (line-seq rdr))))

(defn file-exists? []
  (println (.exists (clojure.java.io/file "Example.txt"))))

(defn my-print-list [list]
  (doseq [item list]
    (println item)))

(defn -main [& args]
  (let [lines  (open-file-lines (first args))
        tokens (lexer lines)]
    (println tokens)
    (let [graph-exp (parser tokens)]
      (println "---")
      (println(filter #(> (count %) 0) graph-exp)))
  ;(my-print-list tokens)
  ))
