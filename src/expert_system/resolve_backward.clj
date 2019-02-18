(load "backward_parser")

(defn find-truable [exps]
  (println exps))

(defn solve-backward [exps facts queries can-be-true]
  true)

(defn resolve-backward [st-parser]
  (let [queries     (:queries st-parser)
        facts       (:facts st-parser)
        exps        (:exps st-parser)
        can-be-true (find-truable exps)
        res         (solve-backward exps facts queries can-be-true)
        ]
    res))


(defn can-be-resolve [exps]
  (and (check-imp exps) (check-all-imply exps)))


(defn resolve-backward-grph [st-parser]
  (if (can-be-resolve (:exps st-parser))
    (resolve-backward st-parser)
    "Invalid proposition. There is more than one implication in one of the proposals"))