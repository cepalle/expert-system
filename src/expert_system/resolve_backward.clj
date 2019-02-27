(load "backward_parser")

(defn exp->equa [exp]
  (if (= (first exp) :impl-right)
    (nth exp 1)
    (last exp)))

(defn index-query [idx exp query]
  (if (re-seq (re-pattern (str query)) (str exp))
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
  (if (check-side-imp exp "right")
    (return-list-truable (last exp) truable)
    (return-list-truable (nth exp 1) truable)))

(defn find-truable [exps truable]
  (map #(return-truable % truable) exps))

(defn remove-from-truable [truable idx]
  (let [seq-t       (seq truable)
        without     (map #(conj (vector (first %)) (remove #{idx} (second %))) seq-t)
        idx-to-rm   (remove false? (map-indexed #(if (empty? (second %2))
                                                   %
                                                   false)
                                                without))
        cleared     (keep-indexed #(if (not (some (fn [elm] (= elm %)) idx-to-rm))
                                     %2)
                                  without)
        res         (into {} cleared)]
    res))

(defn test-prop [str-exp]
  (load-string (let [str-with-and (clojure.string/replace str-exp #":and" "and")
                     str-with-or (clojure.string/replace str-with-and #":or" "or")
                     str-with-not (clojure.string/replace str-with-or #":neg" "not")
                     ; here we recreate the xor func because eval (load-string) can't resolve a local func
                     str-finished (clojure.string/replace str-with-not #":xor" "(fn [bool1 bool2] (if (and bool1 bool2) false (or bool1 bool2)))")]
                 str-finished)))

(defn resolve-recursive [str-exp exps facts query truable idx]
  (let [str-idx (if (> (count str-exp) idx)
                  (nth str-exp idx))
        res (cond
              ; if finished
              (<= (count str-exp) idx) (test-prop str-exp)
              ; if value at idx str-exp contains "(): abc...xyz" return idx++
              (re-matches #"[\(\)\:\ a-z]" (str str-idx)) (resolve-recursive str-exp exps facts query truable (inc idx))
              ; if value is true, replace by true
              (re-seq (re-pattern (str str-idx)) (str facts)) (resolve-recursive (clojure.string/replace str-exp (re-pattern (str str-idx)) "true") exps facts query truable (inc idx))
              ; if value can't be true replace letter by false
              (not (get truable str-idx)) (resolve-recursive (clojure.string/replace str-exp (re-pattern (str str-idx)) "false") exps facts query truable (inc idx))
              ; else means variable can be replaced
              :else (some identity (map #(let [rep-idx   %
                                               new-trua  (remove-from-truable truable rep-idx)
                                               new-str   (clojure.string/replace str-exp (re-pattern (str str-idx)) (pr-str (exp->equa (nth exps rep-idx))))
                                               res       (resolve-recursive new-str exps facts query new-trua idx)]
                                           res)
                                        (get truable str-idx)))
              )
        ]
    res))

(defn solve-backward [exps facts query truable falsable]
  (cond
    (re-seq (re-pattern (str query)) (str facts)) {query true}
    (not (get truable query)) {query false}
    :else {query (boolean (some identity
                                (map #(resolve-recursive (pr-str (exp->equa (nth exps %))) exps facts query truable 0)
                                     (get truable query))))}
    ))

(defn resolve-backward [st-parser]
  (let [queries      (:queries st-parser)
        facts        (:facts st-parser)
        exps         (:exps st-parser)
        truable      (find-truable exps 0)
        falsable     (find-truable exps 1)
        truable-idx  (apply merge (distinct (map #(hash-map % (index-truable % truable)) truable)))
        res          (map #(solve-backward exps facts % truable-idx falsable) queries)
        ]
    res))


(defn resolve-backward-grph [st-parser]
  (if (can-be-resolve (:exps st-parser))
    (resolve-backward st-parser)
    "Invalid proposition. There is more than one implication in one of the proposals"))
