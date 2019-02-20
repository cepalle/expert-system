(load "backward_parser")

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

(defn solve-backward [exps facts queries truable falsable]
  true)

(defn resolve-backward [st-parser]
  (let [queries      (:queries st-parser)
        facts        (:facts st-parser)
        exps         (:exps st-parser)
        truable      (find-truable exps 0)
        debug-1      (println "ok\ncan be" truable)
        falsable     (find-truable exps 1)
        debug-2      (println "ok\ncan be false" falsable)
        res          (solve-backward exps facts queries truable falsable)
        ]
    res))


(defn resolve-backward-grph [st-parser]
  (if (can-be-resolve (:exps st-parser))
    (resolve-backward st-parser)
    "Invalid proposition. There is more than one implication in one of the proposals"))
