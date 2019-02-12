(ns expert-system.core)

(require
 '[clojure.string :as str])

; ----

(defn Example [fileName]
  (println (str "open: " fileName))
  (slurp fileName))

(defn ExampleLine [fileName]
  (println (str "open: " fileName))
  (with-open [rdr (clojure.java.io/reader fileName)]
    (reduce conj [] (line-seq rdr))))

(defn ExampleFileExist []
  (println (.exists (clojure.java.io/file "Example.txt"))))

(defn MyPrint [s]
  (println (str "coucou" s "kookoo")))


(defn LexerLine [line]
  (str/split line #" "))

(defn Lexer [lines]
  (def splitLine (mapcat LexerLine lines))
  (filter
    (fn [x]
      (not (= (count x) 0)))
    splitLine))


(defn MyPrintListe [list]
  (doseq [item list]
    (println item)))

(defn -main [& args]
  (def res (ExampleLine (first args)))
  (MyPrintListe res)
  (def resLexer (Lexer res))
  (MyPrintListe resLexer))

