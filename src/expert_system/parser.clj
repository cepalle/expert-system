(load "util")

(defn exit-parser [nl]
  (println "Parser: invalid syntaxe line:" (inc nl))
  (System/exit 0))

; --- EXP
(defn make-parse-arity-2 [op next]
  (fn parse-fn
    ([nl exps]
     (if (in? exps op)
       (parse-fn '() nl exps)
       (next nl exps)))
    ([exps-befor nl exps-after]
     (let [frst (first exps-after)
           scnd (second exps-after)
           thrd (second (rest exps-after))]
       (cond
         (= thrd nil) (exit-parser nl)
         (and
          (= scnd op)
          (or (char? frst) (list? frst))
          (or (char? thrd) (list? thrd))) (parse-fn nl
                                                    (concat
                                                     (reverse (conj exps-befor (list op frst thrd)))
                                                     (rest (rest (rest exps-after)))))
         :else        (parse-fn (conj exps-befor frst) nl (rest exps-after)))))))

(def parse-equival
  (make-parse-arity-2 :equival
                      (fn [nl exps]
                        (let [n (count exps)]
                          (case n
                            0     exps
                            1     (first exps)
                            (exit-parser nl))))))

(def parse-impl-right (make-parse-arity-2 :impl-right parse-equival))
(def parse-impl-left (make-parse-arity-2 :impl-left parse-impl-right))
(def parse-xor (make-parse-arity-2 :xor parse-impl-left))
(def parse-or (make-parse-arity-2 :or parse-xor))
(def parse-and (make-parse-arity-2 :and parse-or))

(defn parse-neg
  ([nl tokens]
   (if (in? tokens :neg)
     (parse-neg '() nl tokens)
     (parse-and nl tokens)))
  ([exp nl tokens]
   (let [frst (first tokens)
         scnd (second tokens)
         rst  (rest tokens)]
     (cond
       (= frst nil)                                       (parse-neg nl (reverse exp))
       (and (= frst :neg) (= scnd :neg))                  (parse-neg (conj exp frst) nl rst)
       (and (= frst :neg) (or (char? scnd) (list? scnd))) (parse-neg (conj exp (list :neg scnd)) nl (rest rst))
       (= frst :neg)                                      (exit-parser nl)
       :else                                              (parse-neg (conj exp frst) nl rst)))))

(defn parse-exp
  ([nl tokens]
   (if (or (in? tokens :par-open) (in? tokens :par-close))
     (parse-exp '() '() 0 nl tokens)
     (parse-neg nl tokens)))
  ([exp in-par nb-par-open nl tokens]
   (let [frst (first tokens)
         rst  (rest tokens)]
     (cond
       (= frst nil)                                (if (= nb-par-open 0)
                                                     (parse-exp nl (reverse exp))
                                                     (exit-parser nl))
       (and (= frst :par-open) (= nb-par-open 0))  (parse-exp exp in-par (inc nb-par-open) nl rst)
       (and (= frst :par-open) (> nb-par-open 0))  (parse-exp exp (conj in-par frst) (inc nb-par-open) nl rst)
       (and (= frst :par-close) (= nb-par-open 1)) (parse-exp (conj exp (list :par (parse-exp nl (reverse in-par)))) '() (dec nb-par-open) nl rst)
       (and (= frst :par-close) (> nb-par-open 1)) (parse-exp exp (conj in-par frst) (dec nb-par-open) nl rst)
       (and (= frst :par-close) (< nb-par-open 1)) (exit-parser nl)
       (> nb-par-open 0)                           (parse-exp exp (conj in-par frst) nb-par-open nl rst)
       :else                                       (parse-exp (conj exp frst) in-par nb-par-open nl rst)))))

(defn make-parse-simple [tok]
  (fn [nl tokens]
    (let [frst (first tokens)
          rst  (rest tokens)]
      (if (and (= frst tok) (not (some #(not (char? %)) rst)))
        tokens
        (exit-parser nl)))))

(def parse-queries (make-parse-simple :queries))
(def parse-facts (make-parse-simple :facts))

(defn tokens->exp [nl tokens]
  (cond
    (in? tokens :queries)       (parse-queries nl tokens)
    (in? tokens :facts)         (parse-facts nl tokens)
    :else                       (parse-exp nl tokens)))

(defn del-par-exp [exp]
  (cond
    (not (list? exp))    exp
    (= (first exp) :par) (del-par-exp (second exp))
    :else                (map del-par-exp exp)))

; --- PARSER
(defstruct parser-struct :queries :facts :exps)

(defn parser [l-tokens]
  (let [exps        (map-indexed tokens->exp l-tokens)
        not-empty   (filter (fn [exp] (or (char? exp) (> (count exp) 0))) exps)
        res-tmp     (reduce
                     (fn [st exp]
                       (cond
                         (char? exp)              (merge st {:exps (conj (:exps st) exp)})
                         (= (first exp) :queries) (merge st {:queries (concat (:queries st) (rest exp))})
                         (= (first exp) :facts)   (merge st {:facts (concat (:facts st) (rest exp))})
                         :else                    (merge st {:exps (conj (:exps st) exp)})))
                     {:queries '() :facts '() :exps '()} not-empty)
        queries-tmp (seq (set (:queries res-tmp)))
        facts-tmp   (seq (set (:facts res-tmp)))]
    {:queries (if (= queries-tmp nil) '() queries-tmp)
     :facts   (if (= facts-tmp nil) '() facts-tmp)
     :exps    (map del-par-exp (reverse (:exps res-tmp)))}))
