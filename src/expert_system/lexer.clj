(require
 '[clojure.string :as str])

(load "util")

(defn del-com [line]
  (first (str/split line #"#")))

(defn del-space [line]
  (filter (fn [x] (not (in? " \t\n" x))) line))

; TODO handel error

(defn list-char->tokens [list-char]
  (let [frst-char (first list-char)]
    (cond
      (= frst-char nil)                                                                                             '("EOL")
      (= \# frst-char)                                                                                              '("EOL")
      (in? (seq " ") frst-char)                                                                                     (list-char->tokens (rest list-char))
      (in? (seq "=<>") frst-char)                                                                                   (conj (list-char->tokens (rest list-char)) "TODO")
      (in? (seq "()!+|^") frst-char)                                                                                (conj (list-char->tokens (rest list-char)) frst-char)
      (in? (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") frst-char)                                  (conj (list-char->tokens (rest list-char)) frst-char)
      :else                                                                                                         (list-char->tokens (rest list-char)))))

(defn line->tokens [line]
  (list-char->tokens (seq line)))

(defn lines->tokens [lines] (mapcat line->tokens lines))
