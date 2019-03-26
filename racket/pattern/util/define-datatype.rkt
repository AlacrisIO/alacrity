#lang racket/base

(provide define-datatype datatype-out)

(require syntax/parse/define
         (for-syntax racket/base
                     racket/provide-transform
                     racket/struct-info
                     racket/syntax))

(begin-for-syntax
  (define (id/? x) (format-id x "~a?" x #:source x #:props x))
  (define-syntax-class variant
    [pattern (constructor:id field:id ...)
      #:with pred (id/? #'constructor)
      #:with [struct-id ...] #'[constructor]
      #:with [def ...] #'[(struct constructor [field ...] #:transparent)]]
    [pattern id:id
      #:with pred (id/? #'id)
      #:with [struct-id ...] '()
      #:with [def ...] '()])

  (struct datatype-info [predicate struct-ids])
  (define (datatype-or-struct-info? v)
    (or (datatype-info? v) (struct-info? v))))


(define-syntax-parser define-datatype
  [(define-data name:id (constructor:id field:id ...))
   #:when (bound-identifier=? #'name #'constructor)
   #'(struct name [field ...] #:transparent)]
  [(define-data name:id variant:variant ...)
   #:with name? (id/? #'name)
   #'(begin
       (define-syntax name
         (datatype-info (quote-syntax name?)
                        (list (quote-syntax variant.struct-id) ... ...)))
       (define (name? v) (or (variant.pred v) ...))
       variant.def ... ...)])


(define-syntax datatype-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ datatype-id ...)
        #:declare datatype-id
        (static datatype-or-struct-info? "datatype or struct")
        #:with [pred-id ...]
        (for/list ([val (in-list (attribute datatype-id.value))]
                   #:when (datatype-info? val))
          (datatype-info-predicate val))
        #:with [[struct-id ...] ...]
        (for/list ([id (in-list (attribute datatype-id))]
                   [val (in-list (attribute datatype-id.value))])
          (cond [(datatype-info? val) (datatype-info-struct-ids val)]
                [else (list id)]))
        (expand-export
         #'(combine-out datatype-id ...
                        pred-id ...
                        (struct-out struct-id) ... ...)
         modes)]))))

