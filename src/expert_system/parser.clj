(load "util")

(defn graph-eof
  ([tokens]
   (graph-eof '() tokens))
  ([els tokens]
   (let [frst (first tokens)]
     (if (or (= frst :eol) (= frst nil))
       [(reverse els) (rest tokens)]
       (graph-eof (conj els frst) (rest tokens))))))

(defn split-eof
  ([tokens]
   (split-eof '() tokens))
  ([splt tokens]
   (if (= nil (first tokens))
     splt
     (let [[els rest] (graph-eof tokens)]
       (concat (conj splt els) (split-eof rest))))))

(defn parser [tokens]
  (let [splt-eof (split-eof tokens)]
    splt-eof))
