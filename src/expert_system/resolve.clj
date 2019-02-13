(defn my-xor [bool1 bool2]
  (cond
    (and bool1 bool2) false
    :else             (or bool1 bool2)))

(defn my-impl-right [bool1 bool2] (or (not bool1) bool2))
(defn my-impl-left [bool1 bool2] (my-impl-right bool2 bool1))
(defn my-equival [bool1 bool2] (= bool1 bool2))

(defn check-map-var-exp? [map-var exp]
  (if (char? exp)
    (get map-var exp)
    (let [frst (first exp)
          scnd (second exp)
          thrd (second (rest exp))]
      (case frst
        :par        (check-map-var-exp? map-var scnd)
        :neg        (not (check-map-var-exp? map-var scnd))
        :and        (and (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :or         (or (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :xor        (my-xor (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :impl-left  (my-impl-left (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :impl-right (my-impl-right (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :equival    (my-equival (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :else       true))))

(defn check-map-var-exps? [map-var exps]
  (reduce
   (fn [has-false exp]
     (cond
       (has-false)                            false
       (not (check-map-var-exp? map-var exp)) false
       :else                                  (check-map-var-exps? map-var (rest exps))))
   false exps))

; --- MAP VAR
(defn gen-next-map [map-var]
  (let [[key val] (first map-var)]
    (cond
      (or (= key nil) (= val nil)) {}
      val                          (merge (gen-next-map (into (sorted-map) (rest map-var))) {key false})
      :else                        (merge map-var {key true}))))

; --- UTILS
(defn init-map [keys val]
  (cond
    (= (first keys) nil) nil
    :else                (merge {(first keys) val} (init-map (rest keys) val))))

(defn print-all-map-var [map-var]
  (loop [mp-var map-var]
    (let [mp-var-next (gen-next-map mp-var)]
      (println mp-var-next)
      (if (some (fn [[key val]] (= val false)) mp-var-next)
        (recur mp-var-next)))))

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
