(load "util")


(defn graph-eof
  ([tokens]
   (graph-eof '() tokens))
  ([graph tokens]
   (let [frst (first tokens)]
     (if (or (= frst :eol) (= frst nil))
       [(reverse graph) (rest tokens)]
       (graph-eof (conj graph frst) (rest tokens))))))

(defn tokens->graphs
  ([tokens]
   (tokens->graphs '() tokens))
  ([graphs tokens]
   (if (= nil (first tokens))
     graphs
     (let [[graph rest] (graph-eof tokens)]
       (concat (conj graphs graph) (tokens->graphs rest))))))
