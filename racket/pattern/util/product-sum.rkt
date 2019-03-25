#lang racket/base

(provide product product? productof? tuple
         sum sum? sumof? tagged tagged-tuple
         tuple-length tuple-ref)

(require racket/match
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     syntax/transformer))
(module+ test
  (require rackunit))


;; A [Product A ...] is a:
;;   (tuple A ...)
;;
;; tuple : A ... -> [Product A ...]
;; product? : Any -> Bool : [Product _ ...]
;; productof? : [Any -> Bool : A] ... -> [Any -> Bool : [Product A ...]]
;; tuple-length : [Product A ...] -> Natural
;; tuple-ref : [Product A ...] Natural -> [Sum A ...]
;;   where index < (tuple-length tuple)


;; A [Sum A ...] is one of:
;; #:with [i ...] (range (length (attribute A)))
;;  - (tagged i A)
;;  ...
;;
;; tagged : 0 A -> [Sum A _ ...]
;;        ∩ 1 A -> [Sum _ A _ ...]
;;        ∩ 2 A -> [Sum _ _ A _ ...]
;;        ...
;;        ∩ n A -> [Sum _ ...ₙ A _ ...]
;; tagged-tuple : 0 A ... -> [Sum [Product A ...] _ ...]
;;              ∩ 1 A ... -> [Sum _ [Product A ...] _ ...]
;;              ...
;;              ∩ n A ... -> [Sum _ ...ₙ [Product A ...] _ ...]
;; sum? : Any -> Bool : [Sum _ ...]
;; sumof? : [Any -> Bool : A] ... -> [Any -> Bool : [Sum A ...]]


;; ---------------------------------------------------------

(module+ test
  (check-equal? (match (tuple #\a 1 "do")
                  [(tuple letter number solfege)
                   (format "~a, ~a, ~a" letter number solfege)])
                "a, 1, do")
  (check-equal? (match (tagged 3 "mi")
                  [(tagged 3 solfege)
                   solfege])
                "mi")

  (check-pred product? (tuple #\a 1 "do"))
  (check-pred (productof? char? number? string?) (tuple #\a 1 "do"))
  (check-false ((productof? char? number? string?) (tuple #\a 1)))
  (check-false ((productof? char? number? string?) (tuple #\a 1 'notastring)))
  (check-false ((productof? char? number? string?) (tuple #\a 1 "do" 'extra)))
  (check-true (match (tuple #\a 1 "do")
                [(product (? char?) (? number?) (? string?)) #true]
                [_ #false]))

  (check-pred sum? (tagged 0 #\a))
  (check-pred sum? (tagged 3 'entmoot))
  (check-pred (sumof? char? number? string?) (tagged 0 #\a ))
  (check-pred (sumof? char? number? string?) (tagged 1 1))
  (check-pred (sumof? char? number? string?) (tagged 2 "do"))
  (check-false ((sumof? char? number? string?) (tagged 1 #\a)))
  (check-false ((sumof? char? number? string?) (tagged 2 1)))
  (check-false ((sumof? char? number? string?) (tagged 3 'extra)))
  (check-true (match (list (tagged 0 #\a) (tagged 1 1) (tagged 2 "do"))
                [(list (sum (? char?) (? number?) (? string?)) ...) #true]
                [_ #false]))

  (check-equal? (tuple-ref (tuple "a" "b" "c") 0) (tagged 0 "a"))
  (check-equal? (tuple-ref (tuple "a" "b" "c") 1) (tagged 1 "b"))
  (check-equal? (tuple-ref (tuple "a" "b" "c") 2) (tagged 2 "c"))
  (check-exn #rx"expected: index < 3" (λ () (tuple-ref (tuple "a" "b" "c") 3)))

  (check-equal? (match (tagged-tuple 4 'po 'ta 'toes)
                  [(tagged 4 (tuple a b c))
                   (format "~a-~a-~a" a b c)])
                "po-ta-toes")
  (check-equal? (match (tagged 4 (tuple "baked" "mashed"))
                  [(tagged-tuple 4 a b)
                   (format "~a or ~a" a b)])
                "baked or mashed")

  (check-pred (sumof? null? (productof? number? number?)) (tagged-tuple 0))
  (check-pred (sumof? null? (productof? number? number?)) (tagged-tuple 1 9 8))
  (check-false ((sumof? null? (productof? number? number?)) (tagged-tuple 1)))
  (check-false ((sumof? null? (productof? number? number?)) (tagged-tuple 0 2)))
  )

;; ---------------------------------------------------------


(struct tagged [index value] #:transparent)

(define tuple-val
  (let ([tuple (λ elems elems)]) tuple))
(define-match-expander tuple
  (syntax-parser [(_ . stuff) #'(list . stuff)])
  (set!-transformer-procedure (make-variable-like-transformer #'tuple-val)))

(define-match-expander product
  (syntax-parser [(_ . stuff) #'(list . stuff)]))

(define (product? v) (list? v))

(define (tuple-length v)
  (unless (product? v)
    (raise-argument-error 'tuple-length "product" 0 v))
  (length v))

(define-match-expander sum
  (syntax-parser
    [(_ elem-pat:expr ...)
     #:with [i ...] (range (length (attribute elem-pat)))
     #'(or (tagged 'i elem-pat) ...)]))

(define tagged-tuple-val
  (let ([tagged-tuple (λ (tag . elems) (tagged tag elems))]) tagged-tuple))

(define-match-expander tagged-tuple
  (syntax-parser
    [(_ tag-pat:expr . elem-pats)
     #'(tagged tag-pat (list . elem-pats))])
  (set!-transformer-procedure
   (make-variable-like-transformer #'tagged-tuple-val)))

(define (sum? v) (tagged? v))

(define (productof? . elem?s)
  (define n (length elem?s))
  (define (productof?-predicate v)
    (and (product? v)
         (= n (tuple-length v))
         (let loop ([elem?s elem?s] [elems v])
           (match* [elem?s elems]
             [['() '()] #true]
             [[(cons elem? elem?s-rst) (cons elem elems-rst)]
              (and (elem? elem) (loop elem?s-rst elems-rst))]
             [[_ _] #false]))))
  productof?-predicate)

(define (sumof? . elem?s)
  (define n (length elem?s))
  (define (sumof?-predicate v)
    (match v
      [(tagged i elem)
       (and (< i n) ((list-ref elem?s i) elem))]
      [_ #false]))
  sumof?-predicate)

(define (tuple-ref tup i)
  (unless (product? tup)
    (raise-argument-error 'tuple-ref "product" 0 tup i))
  (unless (< i (tuple-length tup))
    (raise-argument-error
     'tuple-ref (format "index < ~v" (tuple-length tup)) 1 tup i))
  (tagged i (list-ref tup i)))

