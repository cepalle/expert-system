(comment
  (defn check-map-var-exp? [map-var exp]
    true)

  (defn check-map-var-exps? [map-var exps]
    (reduce
     (fn [has-false exp]
       (cond
         (has-false)                            false
         (not (check-map-var-exp? map-var exp)) false
         :else                                  (check-map-var-exps? map-var (rest exps))))
     false exps)))

; --- MAP
(defn gen-next-map [map-var]
  (let [[key val] (first map-var)]
    (cond
      (or (= key nil) (= val nil)) {}
      val                          (merge (gen-next-map (into (sorted-map) (rest map-var))) {key false})
      :else                        (merge map-var {key true}))))

(defn print-all-map-var [map-var]
  (loop [mp-var map-var]
    (let [mp-var-next (gen-next-map mp-var)]
      (println mp-var-next)
      (if (some (fn [[key val]] (= val false)) mp-var-next)
        (recur mp-var-next)))))

; --- UTILS
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
        map-true                   (init-map facts true)
        map-var                    (init-map field-in-exps-not-in-facts false)]
    (print-all-map-var map-var)))
