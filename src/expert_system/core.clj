(ns expert-system.core)

(load "lexer")
(load "parser")
(load "resolve")

(defn open-file-lines [fileName]
  (with-open [rdr (clojure.java.io/reader fileName)]
    (reduce conj [] (line-seq rdr))))

(defn file-exists? []
  (println (.exists (clojure.java.io/file "Example.txt"))))

(defn my-print-list [list]
  (doseq [item list]
    (println item)))

(defn print-prop [exps]
  (doseq [[idx exp] (map-indexed vector exps)]
    (println (str "P" idx ":") exp)))

(defn -main [& args]
  (let [lines  (open-file-lines (first args))
        tokens (lexer lines)]
    (println "--- LEXER")
    (println tokens)
    (comment
      (let [st-parser (parser tokens)]
        (println "--- PARSER")
        (println "Queries: " (:queries st-parser))
        (println "Facts: " (:facts st-parser))
        (print-prop (:exps st-parser))
        (let [result (resolve-grph st-parser)]
          (println "--- RESULT")
          (println result))))))
