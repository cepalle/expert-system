(require
 '[clojure.string :as str])

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(def valid-char "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz()!+|^=<>")

(defn letter? [x]
  (in? "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" x))

(defn del-com [line]
  (first (str/split line #"#")))

(defn del-space [line]
  (filter (fn [x] (not (in? " \t\n" x))) line))


(defn get-field-name
  ([line]
   (if (letter? (first line))
     (get-field-name (first line) (rest line))
     ["", line]))
  ([token line]
   (if (letter? (first line))
     (get-field-name (concat token (first line)) (rest line))
     [token line])))

(defn line->token [line]
  (def first-c (first line))
  (cond
    (= first-c nil)        ""
;    (letter? first-c)      (let [[token rest] (get-field-name line)]
;                             (concat token (line->token rest)))
    (in? "()!+|^" first-c) (concat first-c (line->token (rest line)))
    :else                  (concat "@" (line->token (rest line)))))

(defn lines->tokens [lines] (mapcat line->token lines))

