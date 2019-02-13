(defn exp->feilds [exp]
  (map
   (fn [ep]
     (cond
       (list? ep) (exp->feilds ep)
       (char? ep) ep
       :else      nil))
   exp))

(defn epxs->feilds [exps] (filter #(not (= nil %)) (set (flatten (map exp->feilds exps)))))

(defn resolve-grph [st-parser]
  (let [queris    (:queries st-parser)
        facts     (:facts st-parser)
        exps      (:exps st-parser)
        all-field (epxs->feilds exps)]
    all-field))
