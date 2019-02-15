(ns expert-system.core)

(load "lexer")
(load "parser")
(load "resolve_table")
(load "resolve_backward")

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
  (let [frst-arg  (first args)
        file-name (if (= frst-arg nil)
                    (do
                      (println "Usage: lein run ./path/file [-f]")
                      (System/exit 0))
                    frst-arg)
        opt-bool  (= (second args) "-f")
        lines     (try
                    (open-file-lines file-name)
                    (catch Exception ex
                      (do
                        (println "Open file:" file-name "failed")
                        (System/exit 1))))
        l-tokens  (lexer lines)]
    ;(println "--- LEXER")
    ;(my-print-list l-tokens)
    (let [st-parser (parser l-tokens)]
      (println "--- PARSER")
      (println "Queries: " (:queries st-parser))
      (println "Facts: " (:facts st-parser))
      (print-prop (:exps st-parser))

      (let [result (if opt-bool
                     (resolve-table-grph st-parser)
                     (resolve-backward-grph st-parser))]
        (println "--- RESULT")
        (println result)))))
