(require
 '[clojure.string :as str])

(load "util")

; TODO handel error


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
    :par-close))

(defn choose-token-mono-char [c]
  (case c
    \( :par-open
    \) :par-close
    \! :neg
    \+ :and
    \| :or
    \^ :xor
    \? :queries))

(defn list-char->tokens [idx list-char]
  (let [frst-char (first list-char)
        next      #(list-char->tokens idx %)]
    (cond
      (or (= frst-char nil) (= frst-char \#))                                      '(:eol)
      (in? (seq "()!+|^?") frst-char)                                              (conj (next (rest list-char)) (choose-token-mono-char frst-char))
      (in? (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") frst-char) (conj (next (rest list-char)) frst-char)
      (in? (seq "=<>") frst-char)                                                  (conj (next (rest list-char)) "TODO")
      (in? (seq " \t\n") frst-char)                                                (next (rest list-char))
      :else                                                                        (next (rest list-char)))))

(defn line->tokens [[idx line]]
  (list-char->tokens idx (seq line)))

(defn lines->tokens [lines] (mapcat line->tokens (map-indexed vector lines)))
