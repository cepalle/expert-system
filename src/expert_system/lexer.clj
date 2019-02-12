(require
 '[clojure.string :as str])

(load "util")

(defn del-com [line]
  (first (str/split line #"#")))

(defn del-space [line]
  (filter (fn [x] (not (in? " \t\n" x))) line))

; TODO handel error

(defn list-char->tokens [idx list-char]
  (let [frst-char (first list-char)
        next      #(list-char->tokens idx %)]
    (cond
      (= frst-char nil)                                                                                             '("EOL")
      (= \# frst-char)                                                                                              '("EOL")
      (in? (seq " ") frst-char)                                                                                     (next (rest list-char))
      (in? (seq "=<>") frst-char)                                                                                   (conj (next (rest list-char)) "TODO")
      (in? (seq "()!+|^") frst-char)                                                                                (conj (next (rest list-char)) frst-char)
      (in? (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") frst-char)                                  (conj (next (rest list-char)) frst-char)
      :else                                                                                                         (next (rest list-char)))))

(defn line->tokens [[idx line]]
  (list-char->tokens idx (seq line)))

(defn lines->tokens [lines] (mapcat line->tokens (map-indexed vector lines)))
