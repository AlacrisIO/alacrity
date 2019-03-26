#lang racket/base

(provide (datatype-out path-context-step
                       pos-path-tree
                       neg-path-tree))

(require racket/match
         "digest-matcher.rkt"
         (only-in (submod "digest-matcher.rkt" private) deref)
         "util/define-datatype.rkt"
         "util/product-sum.rkt")

;; A PathContext is a [Listof PathContextStep]
;; Where the steps are in "reverse" order. The first element
;; is the most recent one, and the last element is the first
;; one to happen in time.
;; A PathContext is the same whether you're trying to prove
;; or disprove a statement.

;; A PathContextStep is one of:
;;  - (pc-deref)
;;  - (pc-sum-index Natural)
;;  - (pc-product-index Natural)
;;  - (pc-or-index Natural)
;;  - (pc-and-index Natural)
(define-datatype path-context-step
  (pc-deref)
  (pc-sum-index natural)
  (pc-product-index natural)
  (pc-or-index natural)
  (pc-and-index natural))

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
;;  - (pos-deref PosPathTree)
;;  - (sum-index Natural PosPathTree)
;;  - (product-all [Listof PosPathTree])
;;  - (or-index Natural PosPathTree)
;;  - (and-all [Listof PosPathTree])
;;  - (exists-witness Digest PosPathTree)
(define-datatype pos-path-tree
  (trivial)
  (pos-deref next)
  (sum-index natural next)
  (product-all nexts)
  (or-index natural next)
  (and-all nexts)
  (exists-witness value next))

;; A NegPathTree is one of:
;;  - Impossible  ; for `{~and}`, `~pred`, `~refine`, or
;;                ; surface-level mismatches
;;    just run the pattern from here using the trimmed DAG
;;  - (neg-deref NegPathTree)
;;  - (sum-all [Listof NegPathTree])
;;  - (product-index Natural NegPathTree)
;;  - (or-all [Listof NegPathTree])
;;  - (and-index Natural NegPathTree)
;;  - (forall-counterexample Digest NegPathTree)
(define-datatype neg-path-tree
  (impossible)
  (neg-deref next)
  (sum-all nexts)
  (product-index natural next)
  (or-all nexts)
  (and-index natural nexts)
  (forall-counterexample value next))

;; A Counterclaim has:
;;  * Dag
;;  * NegPathTree
;; The Dag should be trimmed to be only what the judge needs
;; for dereferences and the "Impossible" leaves of the
;; NegPathTree.

;; ---------------------------------------------------------

;; A PatternFormula is one of:
;;  - (pat-primitive [Matcher Any (Any ...)])
;;  - (pat-ref PatternFormula)
;;  - (pat-sum [Listof PatternFormula])
;;  - (pat-product [Listof PatternFormula])
;;  - (pat-or [Listof PatternFormula])
;;  - (pat-and [Listof PatternFormula])
;;  - (pat-exists [Any -> PatternFormula])
;;  - (pat-forall [Any -> PatternFormula])
(define-datatype pattern-formula
  (pat-primitive matcher)
  (pat-ref sub)
  (pat-sum subs)
  (pat-product subs)
  (pat-or subs)
  (pat-and subs)
  (pat-exists f)
  (pat-forall f))

;; follow-pos-path can handle formulas with `exists` in them
;; but not `forall`.
(define (follow-pos-path path pat val k)
  (match path
    [(trivial) (k pat val)]
    [(pos-deref next)
     (match pat
       [(pat-ref sub) (follow-pos-path next sub (deref val) k)]
       [_ #f])]
    [(sum-index i next)
     (match* [pat val]
       [[(pat-sum sub-pats) (tagged (== i) sub-val)]
        (follow-pos-path next (list-ref sub-pats i) sub-val k)]
       [[_ _] #f])]
    [(product-all nexts)
     (match pat
       [(pat-product sub-pats)
        (and (= (length nexts) (length sub-pats) (length val))
             (for/and ([next (in-list nexts)]
                       [sub-pat (in-list sub-pats)]
                       [sub-val (in-list val)])
               (follow-pos-path next sub-pat sub-val k)))]
       [_ #f])]
    [(or-index i next)
     (match pat
       [(pat-or sub-pats)
        (follow-pos-path next (list-ref sub-pats i) val k)]
       [_ #f])]
    [(and-all nexts)
     (match pat
       [(pat-and sub-pats)
        (and (= (length nexts) (length sub-pats))
             (for/and ([next (in-list nexts)]
                       [sub-pat (in-list sub-pats)])
               (follow-pos-path next sub-pat val k)))]
       [_ #f])]
    [(exists-witness wit next)
     (match pat
       [(pat-exists f)
        (follow-pos-path next (f wit) val)]
       [_ #f])]
    [_ #f]))

;; follow-neg-path can handle formulas with `forall` in them
;; but not `exists`.
(define (follow-neg-path path pat val k)
  (match path
    [(impossible) (k pat val)]
    [(neg-deref next)
     (match pat
       [(pat-ref sub) (follow-neg-path next sub (deref val) k)]
       [_ #f])]
    [(sum-all nexts)
     (match* [pat val]
       [[(pat-sum sub-pats) (tagged i sub-val)]
        (and (= (length nexts) (length sub-pats))
             (< i (length sub-pats))
             (follow-neg-path (list-ref nexts i)
                              (list-ref sub-pats i)
                              sub-val
                              k))]
       [[_ _] #f])]
    [(product-index i next)
     (match pat
       [(pat-product sub-pats)
        (and (< i (length sub-pats))
             (< i (length val))
             (follow-neg-path next (list-ref sub-pats i) (list-ref val i) k))]
       [_ #f])]
    [(or-all nexts)
     (match pat
       [(pat-or sub-pats)
        (and (= (length nexts) (length sub-pats))
             (for/and ([next (in-list nexts)]
                       [sub-pat (in-list sub-pats)])
               (follow-neg-path next sub-pat val k)))]
       [_ #f])]
    [(and-index i next)
     (match pat
       [(pat-and sub-pats)
        (follow-neg-path next (list-ref sub-pats i) val k)]
       [_ #f])]
    [(forall-counterexample ce next)
     (match pat
       [(pat-forall f)
        (follow-neg-path next (f ce) val)]
       [_ #f])]
    [_ #f]))

