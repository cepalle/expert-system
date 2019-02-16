(load "backward_parser")

(defn check-n-resolve [exps facts queries]
  (if (and (check-imp exps) (check-all-imply exps))
    "Resolved"
    "Invalid proposition. There is more than one implication in one of the proposals"))

(defn resolve-backward-grph [st-parser]
  (let [queries (:queries st-parser)
        facts   (:facts st-parser)
        exps    (:exps st-parser)
        res     (check-n-resolve exps facts queries)
        ]
    res))
