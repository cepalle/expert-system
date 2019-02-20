(load "backward_parser")

(defn return-list-truable [side neg-counter]
  (let [new-count (if (and (seq? side) (= (first side) :neg))
                     (+ 1 neg-counter)
                     neg-counter)
        res (cond
              (and (char? side) (even? neg-counter)) side
              (or (keyword? side) (char? side)) ()
              (not (seq? side)) '(bad)
              :else (flatten (map #(return-list-truable % new-count) side)))]
    res))

(defn return-truable [exp]
  (if (check-side-imp exp "right")
    (return-list-truable (last exp) 0)
    (return-list-truable (nth exp 1) 0)))

(defn find-truable [exps]
  (map return-truable exps))

(defn solve-backward [exps facts queries can-be-true]
  true)

(defn resolve-backward [st-parser]
  (let [queries     (:queries st-parser)
        facts       (:facts st-parser)
        exps        (:exps st-parser)
        debug-2     (println "Before")
        can-be-true (doall (find-truable exps))
        debug-1     (println "ok\ncan be" can-be-true)
        res         (solve-backward exps facts queries can-be-true)
        ]
    res))


(defn resolve-backward-grph [st-parser]
  (if (can-be-resolve (:exps st-parser))
    (resolve-backward st-parser)
    "Invalid proposition. There is more than one implication in one of the proposals"))
