(defn gen-next-map
  ([map-var]
   (gen-next-map 1 map-var))
  ([re map-var]
   (let [[key val] (first map-var)]
     (cond
       (or (= key nil) (= val nil)) {}
       (and (= re 1) (= val 1))     (merge (gen-next-map 1 (into (sorted-map) (rest map-var))) {key 0})
       :else                        (merge map-var {key 1})))))

(defn print-all-map-var [map-var]
  (println map-var)
  (let [mp-var-next (gen-next-map map-var)]
    (if (some (fn [[key val]] (= val 0)) mp-var-next)
      (print-all-map-var mp-var-next))))

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
        map-true                   (init-map facts 1)
        map-var                    (init-map field-in-exps-not-in-facts 0)]
    (print-all-map-var map-var)))
