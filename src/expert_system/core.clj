(ns expert-system.core)

(load "lexer")

(defn open-file-lines [fileName]
  ;(println (str "open: " fileName))
  (with-open [rdr (clojure.java.io/reader fileName)]
    (reduce conj [] (line-seq rdr))))

(defn file-exists? []
  (println (.exists (clojure.java.io/file "Example.txt"))))

(defn my-print-list [list]
  (doseq [item list]
    (println item)))

(defn -main [& args]
  (def lines (open-file-lines (first args)))
  ;(my-print-list lines)
  (def tokens (lines->tokens lines))
  (my-print-list tokens)
  (comment
    (println
     (line->tokens "dlkjndskdjl dqlkdq lejdqs# ljfnsdlfkjn######fkljnfdljksn"))
    (println "---")
    (println
     (line->tokens "#dd"))
    (println "---")
    (println
     (line->tokens "dsds#"))
    (println "---")
    (println
     (line->tokens "dsds#   "))
    (println "---")
    (println
     (line->tokens "dsds#   #####"))
    (println "---")
    (println
     (line->tokens "##dsds#"))))
