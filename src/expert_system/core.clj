(ns expert-system.core)

(require
 '[clojure.string :as str])


(defn open-file-lines [fileName]
  (println (str "open: " fileName))
  (with-open [rdr (clojure.java.io/reader fileName)]
    (reduce conj [] (line-seq rdr))))

(defn file-exists? []
  (println (.exists (clojure.java.io/file "Example.txt"))))

(defn line->tokens [line]
  (str/split line #" "))

(defn lines->tokens [lines]
  (def splitLine (mapcat line->tokens lines))
  (filter
   (fn [x] (not (= (count x) 0)))
   splitLine))


(defn my-print-list [list]
  (doseq [item list]
    (println item)))

(defn -main [& args]
  (def res (open-file-lines (first args)))
  (my-print-list res)
  (def resLexer (lines->tokens res))
  (my-print-list resLexer))
