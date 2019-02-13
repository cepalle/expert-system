(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

; --- use for order
(def tokens
  '(:eol
    :queries
    :facts
    :equival
    :impl-left
    :impl-right
    :xor
    :or
    :and
    :neg
    :par-open
    :par-close
    :par
    :field))
