(defn check-side-imp [exp side]
  (let [to-check (if (= side "right")
                   (last exp)
                   (nth exp 1))
        res (not (or (some #{:xor} (flatten to-check))
                     (some #{:or} (flatten to-check))
                     (some #{:and} (flatten to-check))))
        ]
    res))

(defn imply-only-one [exp]
  (let [expr (first exp)
        res     (cond
                  (= (first expr) :impl-right) (check-side-imp expr "right")
                  (= (first expr) :impl-left) (check-side-imp expr "left")
                  (= (first expr) :equival) (or (check-side-imp expr "right") (check-side-imp expr "left"))
                  :else true
                  )]
    res))

(defn check-all-imply [exps]
  (every? identity (map imply-only-one (map vector exps))))

(defn count-imp [str-exp]
  (+ (count (re-seq #":equival" str-exp))
     (count (re-seq #":impl" str-exp))))

(defn check-imp [exps]
  (= (count exps) (reduce + (for [prop (map vector exps)]
                              (if (or (re-seq #":equival" (str prop)) (re-seq #":impl" (str prop)))
                                (count-imp (str prop))
                                1)))))
