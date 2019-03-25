#lang agile

;; A PathContext is a [Listof PathContextStep]
;; Where the steps are in "reverse" order. The first element
;; is the most recent one, and the last element is the first
;; one to happen in time.
;; A PathContext is the same whether you're trying to prove
;; or disprove a statement.

;; A PathContextStep is one of:
;;  - (deref)
;;  - (list-index Natural)
;;  - (or-index Natural)
;;  - (and-index Natural)

;; TODO: make sum types primitive, and have paths work with
;;       them

;; ---------------------------------------------------------

;; Positive steps vs. negative steps:
;;   Positive steps are used when arguing for a formula,
;;   negative steps are used when arguing against a formula.

;; A PosPathTree or a NegPathTree will be interpreted within
;; a DAG, which will be used for dereferences and for
;; running "elementary" formulas for "Trivial" and
;; "Impossible" leaves.

;; A PosPathTree is one of:
;;  - Trivial     ; for `_`, `{~or}`, `~pred`, `~refine`, or
;;                ; simple atomic data
;;    just run the pattern from here using the trimmed DAG
;;  - (deref PosPathTree)
;;  - (list-all [Listof PosPathTree])
;;  - (or-index Natural PosPathTree)
;;  - (and-all [Listof PosPathTree])
;;  - (exists-witness Digest PosPathTree)

;; A NegPathTree is one of:
;;  - Impossible  ; for `{~and}`, `~pred`, `~refine`, or
;;                ; surface-level mismatches
;;    just run the pattern from here using the trimmed DAG
;;  - (deref NegPathTree)
;;  - (list-index Natural NegPathTree)
;;  - (or-all [Listof NegPathTree])
;;  - (and-index Natural NegPathTree)
;;  - (forall-counterexample Digest NegPathTree)

;; A Counterclaim has:
;;  * Dag
;;  * NegPathTree
;; The Dag should be trimmed to be only what the judge needs
;; for dereferences and the "Impossible" leaves of the
;; NegPathTree.

;; ---------------------------------------------------------
