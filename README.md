# expert-system

## dependencies

- Java 8+
- clojure
- leiningen

## run

example:

```
lein run test/good/12
```

### RESOLVER

A resolver take a parser-struct in input:

```clojure
(defstruct parser-struct :queries :facts :exps)

(def example-parser-struct {
  :queries '(\A \B \C) ; list of char
  :facts '(\E \H \J)   ; list of char
  :exps '(             ; list of exp
  \A
  (:neg \B)
  (:equival (:or \D \E) \F)
  \C
  (:impl-right (:par (:equival (:neg (:par (:or \A \B))) (:and (:neg \A) (:neg \B)))) \G)
  )
})

(comment                  ; exp:
  (char)                  ; ABCdefg...
  (:par exp)              ; (exp)
  (:neg exp)              ; !exp
  (:and exp1 exp2)        ; exp1 + exp2
  (:or exp1 exp2)         ; exp1 | exp2
  (:xor exp1 exp2)        ; exp1 ^ exp2
  (:impl-right exp1 exp2) ; exp1 => exp2
  (:impl-left exp1 exp2)  ; exp1 <= exp2
  (:equival exp1 exp2)    ; exp1 <=> exp2
)

; EXAMPLE FOR CHECK A EXP

(defn check-map-var-exp? [map-var exp]
  (if (char? exp)
    (get map-var exp)
    (let [frst (first exp)
          scnd (second exp)
          thrd (second (rest exp))]
      (case frst
        :par        (check-map-var-exp? map-var scnd)
        :neg        (not (check-map-var-exp? map-var scnd))
        :and        (and (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :or         (or (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :xor        (my-xor (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :impl-left  (my-impl-left (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :impl-right (my-impl-right (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :equival    (= (check-map-var-exp? map-var scnd) (check-map-var-exp? map-var thrd))
        :else       true))))

; Where map-var:
; Map<char, bool>
(def example-map-var {
  \A true
  \B true
  \C true
  \H false
  \O true
  \I false
})
```

A resolver output is a dictionary with key is queries values:

```clojure
; Map<char, bool | nil>
(def example-result {
  \A true   ; True
  \B false  ; False
  \C nil    ; Undetermined
})
```
