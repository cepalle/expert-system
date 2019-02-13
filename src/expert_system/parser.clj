(load "util")

; --- EXP
(defn parse-neg
  ([with-par] with-par)
  ([]))

(defn parse-par
  ([tokens] tokens)
  ([]))

(defn tokens->exp [tokens]
  (let [with-par (parse-par tokens)
        with-neg (parse-neg with-par)]
    with-neg))

; --- EOF
(defn graph-eof
  ([tokens]
   (graph-eof '() tokens))
  ([els tokens]
   (let [frst (first tokens)]
     (if (or (= frst :eol) (= frst nil))
       [(reverse els) (rest tokens)]
       (graph-eof (conj els frst) (rest tokens))))))

(defn split-eof
  ([tokens]
   (split-eof '() tokens))
  ([splt tokens]
   (if (= nil (first tokens))
     splt
     (let [[els rest] (graph-eof tokens)]
       (concat (conj splt els) (split-eof rest))))))

; --- PARSER
(defn parser [tokens]
  (let [splt-eof (split-eof tokens)]
    (map tokens->exp splt-eof)))
