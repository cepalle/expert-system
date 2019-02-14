(load "util")

; --- EXP
(defn make-parse-arity-2 [op next]
  (fn parse-fn
    ([exps]
     (if (in? exps op)
       (parse-fn '() exps)
       (next exps)))
    ([exps-befor exps-after]
     (let [frst (first exps-after)
           scnd (second exps-after)
           thrd (second (rest exps-after))]
       (cond
         (= thrd nil)  (parse-fn (reverse exps-befor))
         (= scnd op)   (parse-fn
                        (concat (reverse (conj exps-befor (list op frst thrd))) (rest (rest (rest exps-after)))))
         :else         (parse-fn (conj exps-befor frst) (rest exps-after)))))))

(def parse-equival (make-parse-arity-2 :equival identity))
(def parse-impl-right (make-parse-arity-2 :impl-right parse-equival))
(def parse-impl-left (make-parse-arity-2 :impl-left parse-impl-right))
(def parse-xor (make-parse-arity-2 :xor parse-impl-left))
(def parse-or (make-parse-arity-2 :or parse-xor))
(def parse-and (make-parse-arity-2 :and parse-or))

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
     (first (parse-neg tokens))))
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

(defn tokens->exp [tokens]
  (cond
    (= (first tokens) :queries) tokens
    (= (first tokens) :facts)   tokens
    :else                       (parse-par tokens)))

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

(defstruct parser-struct :queries :facts :exps)

(defn parser [tokens]
  (let [splt-eof (split-eof tokens)]
    (let [exps   (filter #(or (char? %1) (> (count %1) 0)) (map tokens->exp splt-eof))
          st-rev (reduce
                  (fn [st exp]
                    (cond
                      (char? exp)              (struct parser-struct (:queries st) (:facts st) (conj (:exps st) exp))
                      (= (first exp) :queries) (struct parser-struct (rest exp) (:facts st) (:exps st))
                      (= (first exp) :facts)   (struct parser-struct (:queries st) (rest exp) (:exps st))
                      :else                    (struct parser-struct (:queries st) (:facts st) (conj (:exps st) exp))))
                  (struct parser-struct '() '() '())
                  exps)]
      (struct parser-struct (:queries st-rev) (:facts st-rev) (reverse (:exps st-rev))))))
