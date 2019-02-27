(defn my-xor [bool1 bool2]
  (cond
    (and bool1 bool2) false
    :else             (or bool1 bool2)))

(defn my-impl-right [bool1 bool2] (or (not bool1) bool2))
(defn my-impl-left [bool1 bool2] (my-impl-right bool2 bool1))
