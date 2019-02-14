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

(declare seq-c->tokens)

(defn get-token-multi-char [nl cl seq-c]
  (let [frst-char (first seq-c)
        scnd-char (second seq-c)
        thrd-char (second (rest seq-c))]
    (cond
      (and (= frst-char \<) (= scnd-char \=) (= thrd-char \>)) (conj (seq-c->tokens nl (+ cl 3) (rest (rest (rest seq-c)))) :equival)
      (and (= frst-char \<) (= scnd-char \=))                  (conj (seq-c->tokens nl (+ cl 2) (rest (rest seq-c))) :impl-left)
      (and (= frst-char \=) (= scnd-char \>))                  (conj (seq-c->tokens nl (+ cl 2) (rest (rest seq-c))) :impl-right)
      (= frst-char \=)                                         (conj (seq-c->tokens nl (inc cl) (rest seq-c)) :facts)
      :else                                                    (do
                                                                 (println "Lexer: invalid char line:" (inc nl) "col:" (inc cl))
                                                                 (System/exit 0)))))

(defn seq-c->tokens
  ([nl seq-c]
   (seq-c->tokens nl 0 seq-c))
  ([nl cl seq-c]
   (let [frst      (first seq-c)]
     (cond
       (= frst nil)                                                            '()
       (in? (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") frst) (conj (seq-c->tokens nl (inc cl) (rest seq-c)) frst)
       (in? (seq "()!+|^?") frst)                                              (conj (seq-c->tokens nl (inc cl) (rest seq-c)) (choose-token-mono-char frst))
       (in? (seq "=<>") frst)                                                  (get-token-multi-char nl cl seq-c)
       (in? (seq " ") frst)                                                    (seq-c->tokens nl (inc cl) (rest seq-c))
       :else                                                                   (do
                                                                                 (println "Lexer: invalid char line:" (inc nl) "col:" (inc cl))
                                                                                 (System/exit 0))))))

(defn del-coms [seq-c]
  (let [frst (first seq-c)]
    (if (or (= frst nil) (= frst \#))
      '()
      (conj (del-coms (rest seq-c)) frst))))

(defn line->tokens [nl line] (seq-c->tokens nl (del-coms (seq line))))

(defn lexer [lines] (map-indexed line->tokens lines))
