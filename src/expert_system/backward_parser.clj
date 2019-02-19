
(defn exit-message [msg]
  (println msg "If you want to activate bypass this error message, add '-f'")
  (System/exit 0))

(defn check-side-imp [exp side]
  (let [to-check (if (= side "right")
                   (last exp)
                   (nth exp 1))
        res (not (or (some #{:xor} (flatten to-check))
                     (some #{:or} (flatten to-check))))
        ]
    res))

(defn good-imply [exp]
  (let [expr (first exp)
        res     (cond
                  (= (first expr) :impl-right) (check-side-imp expr "right")
                  (= (first expr) :impl-left) (check-side-imp expr "left")
                  :else false
                  )]
    res))

(defn check-all-imply [exps]
  (every? identity (map good-imply (map vector exps))))

(defn imply-line [prop]
  (= 1 (count (re-seq #":impl" (str prop)))))

(defn count-imp-line [exps]
  (every? identity (map #(= 1 (count (re-seq #":impl" (str %)))) (map vector exps))))

(defn contains-equiv [exps]
  (re-seq #":equival" (str (list exps))))

(defn can-be-resolve [exps]
  (cond
    (contains-equiv exps) (exit-message "Error, there is an equivalence.")
    (not (count-imp-line exps)) (exit-message "Error, there are too much/is not enough implication(s) on the same line.")
    (not (good-imply exps)) (exit-message "Error, check what's there after your implication.")
    :else true))
