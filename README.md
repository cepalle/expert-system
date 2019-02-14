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

## Resolver

In src/expert_system/core.clj line 44: change `resolve-grph` for use your resolver.

Resolver input:

```clojure
(def example-parser-input {
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
```

A resolver output is a dictionary where key is queries values:

```clojure
; Map<char, bool | nil>
(def example-result {
  \A true   ; True
  \B false  ; False
  \C nil    ; Undetermined
})
```
