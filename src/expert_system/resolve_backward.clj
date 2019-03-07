(load "backward_parser")

(defn exp->equa [exp]
  (if (= (first exp) :impl-right)
    (nth exp 1)
    (last exp)))

(defn index-query [idx exp query]
  (if (re-seq (re-pattern (str query)) (pr-str exp))
    idx))

(defn index-truable [query truable]
  (remove not (map-indexed #(index-query % %2 query) truable)))

(defn return-list-truable [side neg-counter]
  (let [new-count (if (and (seq? side) (= (first side) :neg))
                    (+ 1 neg-counter)
                    neg-counter)
        res (cond
              (and (char? side) (even? neg-counter)) side
              (or (keyword? side) (char? side)) ()
              :else (flatten (map #(return-list-truable % new-count) side)))]
    res))

(defn return-truable [exp truable]
  (if (= (first exp) :impl-right)
    (return-list-truable (last exp) truable)
    (return-list-truable (nth exp 1) truable)))

(defn find-truable [exps truable]
  (map #(return-truable % truable) exps))

(defn remove-from-truable [truable idx query]
  (let [removed (assoc-in truable (vector query) (remove #{idx} (get truable query)))
        to-rm (remove not (map #(if (empty? (get removed %)) %) (keys removed)))
        res (apply dissoc removed to-rm)]
        res))

(defn test-prop [str-exp]
  (load-string (let [str-with-and (clojure.string/replace str-exp #":and" "and")
                     str-with-or (clojure.string/replace str-with-and #":or" "or")
                     str-with-not (clojure.string/replace str-with-or #":neg" "not")
                     ; here we recreate the xor func because eval (load-string) can't resolve a local func
                     str-finished (clojure.string/replace str-with-not #":xor" "(fn [bool1 bool2] (if (and bool1 bool2) false (or bool1 bool2)))")]
                 str-finished)))

(defn resolve-recursive [str-exp exps facts query truable idx is-true]
  (let [char-idx (if (> (count str-exp) idx)
                  (nth str-exp idx)
                  nil)
        res (cond
              ; if finished
              (or (<= (count str-exp) idx) (boolean (re-matches #"^[\(\)\:\ a-z]{1,}$" str-exp))) (test-prop str-exp)
              ; if value at idx str-exp contains "(): abc...xyz" return idx++
              (re-matches #"[\(\)\:\ a-z]" (str char-idx)) (resolve-recursive str-exp exps facts query truable (inc idx) is-true)
              ; if value is true, replace by true
              (or (re-seq (re-pattern (str char-idx)) (str facts))
                  (re-seq (re-pattern (str char-idx)) (pr-str is-true))) (resolve-recursive (clojure.string/replace str-exp (re-pattern (str char-idx)) "true") exps facts query truable (inc idx) is-true)
              ; if value can't be true replace letter by false
              (not (get truable char-idx)) (resolve-recursive (clojure.string/replace str-exp (re-pattern (str char-idx)) "false") exps facts query truable (inc idx) is-true)
              ; else means variable can be replaced
              :else (some identity (map #(let [rep-idx   %
                                               new-trua    (remove-from-truable truable rep-idx query)
                                               new-trua-b  (remove-from-truable new-trua rep-idx char-idx)
                                               new-str     (clojure.string/replace str-exp (re-pattern (str char-idx)) (pr-str (exp->equa (nth exps rep-idx))))
                                               new-is-tr   (if (resolve-recursive (pr-str (exp->equa (nth exps rep-idx))) exps facts char-idx new-trua-b 0 is-true)
                                                             (concat is-true (list char-idx))
                                                             is-true)
                                               res         (resolve-recursive new-str exps facts query new-trua-b idx new-is-tr)]
                                           res)
                                        (get truable char-idx)))
              )
        ]
    res))

(defn solve-backward [exps facts query truable falsable]
  (cond
    (re-seq (re-pattern (str query)) (str facts)) {query true}
    (not (get truable query)) {query false}
    :else {query (boolean (some identity
                                (map #(let [new-trua (remove-from-truable truable % query)
                                            res (resolve-recursive (pr-str (exp->equa (nth exps %))) exps facts query new-trua 0 '())]
                                       res)
                                     (get truable query))))}
    ))

(defn resolve-backward [st-parser]
  (let [queries      (:queries st-parser)
        facts        (:facts st-parser)
        exps         (:exps st-parser)
        truable      (find-truable exps 0)
        falsable     (find-truable exps 1)
        truable-idx  (apply merge (distinct (map #(hash-map % (index-truable % truable)) (flatten truable))))
        res          (map #(solve-backward exps facts % truable-idx falsable) queries)
        ]
    res))


(defn resolve-backward-grph [st-parser]
  (if (can-be-resolve (:exps st-parser))
    (resolve-backward st-parser)
    "Invalid proposition. There is more than one implication in one of the proposals"))
