(load "util")

(defn choose-token-mono-char [c]
  (case c
    \( :par-open
    \) :par-close
    \! :neg
    \+ :and
    \| :or
    \^ :xor
    \? :queries))

(defn get-token-multi-char [list-char next]
  (let [frst-char (first list-char)
        scnd-char (second list-char)
        thrd-char (second (rest list-char))]
    (cond
      (and (= frst-char \<) (= scnd-char \=) (= thrd-char \>)) (conj (next (rest (rest list-char))) :equival)
      (and (= frst-char \<) (= scnd-char \=))                  (conj (next (rest (rest list-char))) :impl-left)
      (and (= frst-char \=) (= scnd-char \>))                  (conj (next (rest (rest list-char))) :impl-right)
      (= frst-char \=)                                         (conj (next (rest list-char)) :queries)
      :else                                                    (next (rest list-char)))))

(defn list-char->tokens [idx list-char]
  (let [frst-char (first list-char)
        next      #(list-char->tokens idx %)]
    (cond
      (or (= frst-char nil) (= frst-char \#))                                      '(:eol)
      (in? (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") frst-char) (conj (next (rest list-char)) frst-char)
      (in? (seq "()!+|^?") frst-char)                                              (conj (next (rest list-char)) (choose-token-mono-char frst-char))
      (in? (seq "=<>") frst-char)                                                  (get-token-multi-char list-char next)
      (in? (seq " \t\n") frst-char)                                                (next (rest list-char))
      :else                                                                        (next (rest list-char)))))

(defn line->tokens [[idx line]]
  (list-char->tokens idx (seq line)))

(defn lines->tokens [lines] (mapcat line->tokens (map-indexed vector lines)))
