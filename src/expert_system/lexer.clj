(require
 '[clojure.string :as str])

(defn line->tokens [line]
  (str/split line #" "))

(defn lines->tokens [lines]
  (def splitLine (mapcat line->tokens lines))
  splitLine)
