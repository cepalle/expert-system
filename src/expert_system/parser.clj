(load "util")

; --- EXP
(defn parse-and
  ([with-neg]
   (if (in? with-neg :and)
     (parse-and '() with-neg)
     with-neg))
  ([exp with-neg]
   (let [frst (first with-neg)
         scnd (second with-neg)
         thrd (second (rest with-neg))]
     (cond
       (= thrd nil)  (parse-and (reverse exp))
       (= scnd :and) (parse-and
                       (concat (reverse (conj exp (list :and frst thrd))) (rest (rest (rest with-neg)))))
       :else         (parse-and (conj exp frst) (rest with-neg))))))

(defn parse-neg
  ([with-par]
   (if (in? with-par :neg)
     (parse-neg '() with-par)
     (parse-and with-par)))
  ([exp with-par]
   (let [frst (first with-par)
         rst  (rest with-par)]
     (cond
       (= frst nil)                                   (parse-neg (reverse exp))
       (and (= frst :neg) (not (= (first rst) :neg))) (parse-neg (conj exp (list :neg (first rst))) (rest rst))
       :else                                          (parse-neg (conj exp frst) rst)))))

(defn parse-par
  ([tokens]
   (if (in? tokens :par-open)
     (parse-par '() '() 0 tokens)
     (parse-neg tokens)))
  ([exp in-par nb-par-open tokens]
   (let [frst (first tokens)
         rst  (rest tokens)]
     (cond
       (= frst nil)                                (parse-par (reverse exp))
       (and (= frst :par-open) (= nb-par-open 0))  (parse-par exp in-par (inc nb-par-open) rst)
       (and (= frst :par-open) (> nb-par-open 0))  (parse-par exp (conj in-par frst) (inc nb-par-open) rst)
       (and (= frst :par-close) (= nb-par-open 1)) (parse-par (conj exp (list :par (parse-par (reverse in-par)))) '() (dec nb-par-open) rst)
       (and (= frst :par-close) (> nb-par-open 1)) (parse-par exp (conj in-par frst) (dec nb-par-open) rst)
       (> nb-par-open 0)                           (parse-par exp (conj in-par frst) nb-par-open rst)
       :else                                       (parse-par (conj exp frst) in-par nb-par-open rst)))))

(def tokens->exp parse-par)

; --- EOF
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

; --- PARSER
(defn parser [tokens]
  (let [splt-eof (split-eof tokens)]
    (map tokens->exp splt-eof)))
