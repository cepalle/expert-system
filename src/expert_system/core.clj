(ns expert-system.core)

(defn Example [fileName]
  (println (str "open: " fileName))
  (slurp fileName))

(defn ExampleLine [fileName]
  (println (str "open: " fileName))
  (with-open [rdr (clojure.java.io/reader fileName)]
    (reduce conj [] (line-seq rdr))))

(defn ExampleFileExist []
  (println (.exists (clojure.java.io/file "Example.txt"))))

(defn -main [& args]
  (println (ExampleLine (first args))))

