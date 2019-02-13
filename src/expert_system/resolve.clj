(defn init-map [keys val]
  (cond
    (= (first keys) nil) nil
    :else                (merge {(first keys) val} (init-map (rest keys) val))))

; --- EXTRACT FIELD
(defn exp->fields [exp]
  (map
   (fn [ep]
     (cond
       (list? ep) (exp->fields ep)
       (char? ep) ep
       :else      nil))
   exp))

(defn epxs->fields [exps]
  (filter #(not (= nil %)) (set (flatten (map exp->fields exps)))))

; --- RESOLVE
(defn resolve-grph [st-parser]
  (let [queris                     (:queries st-parser)
        facts                      (:facts st-parser)
        exps                       (:exps st-parser)
        field-in-exps              (epxs->fields exps)
        field-in-exps-not-in-facts (filter #(not (in? facts %)) field-in-exps)
        map-true                   (init-map facts 1)
        map-var                    (init-map field-in-exps-not-in-facts 0)]
    (merge map-var map-true)))
