(require
 '[clojure.string :as str])

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(def valid-char "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz()!+|^=<>")

(defn del-com [line]
  (first (str/split line #"#")))

(defn del-space [line]
  (filter (fn [x] (not (in? " \t\n" x))) line))


(defn lines->tokens [lines]
  (def ls-del-com (map del-com lines))
  (def ls-del-space (map del-space ls-del-com))

  ls-del-space)

