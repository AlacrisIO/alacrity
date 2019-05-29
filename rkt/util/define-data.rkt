#lang racket/base

(provide define-data)

(require syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

;; (define-data type-name
;;   (variant-name field-name ...)
;;   ...)
;; Defines:
;;  * type-name? as a predicate for the union of the structs
;;  * each variant-name as a struct
;;  * each variant-name? as a predicate for the struct
;;  * each variant-name-field-name as an accessor
;; If there is exactly one variant clause, the type-name and
;; the variant-name can be the same

(begin-for-syntax
  (define (id/? x) (format-id x "~a?" x #:source x #:props x))
  (define-syntax-class variant
    [pattern (constructor:id field:id ...)
      #:with pred (id/? #'constructor)
      #:with [def ...] #'[(struct constructor [field ...] #:transparent)]]
    [pattern id:id
      #:with pred (id/? #'id)
      #:with [def ...] '()]))

(define-syntax-parser define-data
  [(define-data name:id (constructor:id field:id ...))
   #:when (bound-identifier=? #'name #'constructor)
   #'(struct name [field ...] #:transparent)]
  [(define-data name:id variant:variant ...)
   #:with name? (id/? #'name)
   #'(begin (define (name? v) (or (variant.pred v) ...)) variant.def ... ...)])

