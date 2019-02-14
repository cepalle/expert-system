(defn my-xor [bool1 bool2]
  (cond
    (and bool1 bool2) false
    :else             (or bool1 bool2)))

(defn my-impl-right [bool1 bool2] (or (not bool1) bool2))
(defn my-impl-left [bool1 bool2] (my-impl-right bool2 bool1))

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
        :equival    (= (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))))))

(defn check-map-var-exps? [map-var exps]
  (not
   (reduce
    (fn [has-false exp]
      (if has-false
        true
        (not (check-map-var-exp? map-var exp))))
    false exps)))

; --- MAP VAR
(defn gen-next-map [map-var]
  (let [[key val] (first map-var)]
    (cond
      (or (= key nil) (= val nil)) {}
      val                          (merge (gen-next-map (into (sorted-map) (rest map-var))) {key false})
      :else                        (merge map-var {key true}))))

(defn all-map-var-callback [map-var callback]
  (callback map-var)
  (loop [mp-var map-var]
    (let [mp-var-next (gen-next-map mp-var)]
      (callback mp-var-next)
      (if (some (fn [[key val]] (= val false)) mp-var-next)
        (recur mp-var-next)))))

; --- UTILS
(defn init-map [keys val]
  (cond
    (= (first keys) nil) nil
    :else                (merge {(first keys) val} (init-map (rest keys) val))))

; --- EXTRACT FIELD
(defn exp->fields [exp]
  (cond
    (char? exp) exp
    (list? exp) (map exp->fields exp)
    :else       nil))

(defn epxs->fields [exps]
  (filter #(not (= nil %)) (set (flatten (map exp->fields exps)))))

; --- RESOLVE

(defn get-maps-good [map-var-init map-true exps]
  (loop [maps-good '()
         map-var   map-var-init]
    (let [maps-good-next (if (check-map-var-exps? (merge map-var map-true) exps)
                           (conj maps-good map-var)
                           maps-good)]
      (if (some (fn [[key val]] (= val false)) map-var)
        (recur maps-good-next (gen-next-map map-var))
        maps-good-next))))

(defn get-queris-res-recur [map-res queris]
  (let [frst (first queris)
        next #(get-queris-res-recur map-res (rest queris))]
    (if (= frst nil)
      {}
      (cond
        (some #(= (get % frst) nil) map-res)         (merge {frst nil} (next))
        (not (some #(= (get % frst) true) map-res))  (merge {frst false} (next))
        (not (some #(= (get % frst) false) map-res)) (merge {frst true} (next))
        :else                                        (merge {frst nil} (next))))))

(defn get-queris-res [map-res queris]
  (if (= (count map-res) 0)
    {"INVALID PROPOSITION" nil}
    (get-queris-res-recur map-res queris)))

(defn resolve-grph [st-parser]
  (let [queris                     (:queries st-parser)
        facts                      (:facts st-parser)
        exps                       (:exps st-parser)
        field-in-exps              (epxs->fields exps)
        field-in-exps-not-in-facts (filter #(not (in? facts %)) field-in-exps)
        map-true                   (init-map facts true)
        map-var                    (init-map field-in-exps-not-in-facts false)
        maps-var-good              (get-maps-good map-var map-true exps)
        map-res                    (map #(merge % map-true) maps-var-good)
        res                        (get-queris-res map-res queris)]
    res))
