(load "util")

; --- EXP
(defn parse-neg
  ([with-par] with-par)
  ([]))

(defn parse-par
  ([tokens]
   (if (in? tokens :par-open)
     (parse-par '() '() 0 tokens)
     (parse-neg tokens)))
  ([exp in-par nb-par-open tokens]
   (let [frst (first tokens)
         rst  (rest tokens)]
     (cond
       (= frst nil)                                (reverse exp)
       (and (= frst :par-open) (= nb-par-open 0))  (parse-par exp in-par (inc nb-par-open) rst)
       (and (= frst :par-open) (> nb-par-open 0))  (parse-par exp (conj in-par frst) (inc nb-par-open) rst)
       (and (= frst :par-close) (= nb-par-open 1)) (parse-par (conj exp (list :par (parse-par (reverse in-par)))) '() (dec nb-par-open) rst)
       (and (= frst :par-close) (> nb-par-open 1)) (parse-par exp (conj in-par frst) (dec nb-par-open) rst)
       (> nb-par-open 0)                           (parse-par exp (conj in-par frst) nb-par-open rst)
       :else                                       (parse-par (conj exp frst) in-par nb-par-open rst)))))

(defn tokens->exp [tokens]
  (let [with-par (parse-par tokens)
        with-neg (parse-neg with-par)]
    with-neg))

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
