(load "backward_parser")

(defn return-truable [exp]
  (cond
    (= (first exp) :impl-right) (first exp)
    (= (first exp) :impl-left) (last exp)
    (= (first exp) :equival) true
    :else true))

(defn find-truable [exps]
  (map return-truable exps))

(defn solve-backward [exps facts queries can-be-true]
  true)

(defn resolve-backward [st-parser]
  (let [queries     (:queries st-parser)
        facts       (:facts st-parser)
        exps        (:exps st-parser)
        can-be-true (find-truable exps)
        debug       (println "can be" can-be-true)
        res         (solve-backward exps facts queries can-be-true)
        ]
    res))


(defn can-be-resolve [exps]
  (and (check-imp exps) (check-all-imply exps)))


(defn resolve-backward-grph [st-parser]
  (if (can-be-resolve (:exps st-parser))
    (resolve-backward st-parser)
    "Invalid proposition. There is more than one implication in one of the proposals"))