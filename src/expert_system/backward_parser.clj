(defn check-imp-right [exp]
  (or (not (list? (nth exp 2)))
      (and (= (count (nth exp 2)) 2) (= (nth (nth exp 2) 0) :neg))))

(defn check-imp-left [exp]
  (or (not (list? (nth exp 1)))
      (and (= (count (nth exp 1)) 2) (= (nth (nth exp 1) 0) :neg))))

(defn check-equival [exp]
  (or (check-imp-left exp) (check-imp-right exp)))

(defn imply-only-one [exp]
  (let [expr (nth exp 0)
        res     (cond
                  (= (nth expr 0) :impl-left) (check-imp-left expr)
                  (= (nth expr 0) :impl-right) (check-imp-right expr)
                  (= (nth expr 0) :equival) (check-equival expr)
                  :else true
                  )]
    res))

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
