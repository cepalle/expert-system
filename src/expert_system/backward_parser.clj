(defn check-imp-right [exp]
  (not (list? (nth exp 2))))

(defn check-imp-left [exp]
  (not (list? (nth exp 1))))

(defn check-equival [exp]
  (or (check-imp-left exp) (check-imp-right exp)))

(defn imply-only-one [exp]
  (cond
    (= (nth exp 0) :impl-left) (check-imp-left exp)
    (= (nth exp 0) :impl-right) (check-imp-right exp)
    (= (nth exp 0) :equival) (check-equival exp)
    :else true
    ))

(defn check-all-imply [exps]
  (every? identity (for [prop (map vector exps)]
                     (imply-only-one prop))))

(defn count-imp [str-exp]
  (+ (count (re-seq #":equival" str-exp))
     (count (re-seq #":impl" str-exp))))

(defn check-imp [exps]
  (= (count exps) (reduce + (for [prop (map vector exps)]
                              (if (or (re-seq #":equival" (str prop)) (re-seq #":impl" (str prop)))
                                (count-imp (str prop))
                                1)))))
