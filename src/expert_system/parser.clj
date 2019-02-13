(load "util")

(defn graph-xor
  ([tokens]
   (if (some #(= :xor %) tokens)
     ()
     (graph-neg tokens)))
  ([graph tokens]))

(defn graph-or
  ([tokens]
   (if (some #(= :or %) tokens)
     ()
     (graph-xor tokens)))
  ([graph tokens]))

(defn graph-and
  ([tokens]
   (if (some #(= :and %) tokens)
     ()
     (graph-or tokens)))
  ([graph tokens]))

(defn graph-neg
  ([tokens]
   (if (some #(= :neg %) tokens)
     ()
     (graph-and tokens)))
  ([graph tokens]))

(defn graph-par
  ([tokens]
   (if (some #(= :par-open %) tokens)
     (graph-par '(:par) tokens)
     (graph-neg tokens)))
  ([graph tokens]))

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
