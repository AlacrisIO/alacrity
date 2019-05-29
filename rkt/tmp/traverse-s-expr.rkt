#lang racket/base

(provide traverse-s-expr/recur)

(require racket/match)

;; traverse-s-expr/recur : S-Expr Natural [S-Expr -> S-Expr] -> S-Expr
(define (traverse-s-expr/recur e qqd f)
  (cond
    [(zero? qqd)
     (match e
       [(? symbol? x)  x]
       [(? boolean? b) b]
       [(? number? n)  n]
       [(? string? s)  s]
       [`(,'quote ,_)  e]
       [`(,'quasiquote ,d)
        `(,'quasiquote ,(traverse-s-expr/recur d 1 f))]
       [`(,a . ,bs)
        `(,(f a) . ,(map f bs))])]
    [else
     (match e
       [`(,'quasiquote ,d)
        `(,'quasiquote ,(traverse-s-expr/recur d (add1 qqd) f))]
       [`(,'unquote ,e)
        #:when (= 1 qqd)
        `(,'unquote ,(f e))]
       [`(,'unquote-splicing ,e)
        #:when (= 1 qqd)
        `(,'unquote-splicing ,(f e))]
       [`(,'unquote ,d)
        `(,'unquote ,(traverse-s-expr/recur d (sub1 qqd) f))]
       [`(,'unquote-splicing ,d)
        `(,'unquote-splicing ,(traverse-s-expr/recur d (sub1 qqd) f))]
       [`(,d ...)
        (for/list ([d (in-list d)])
          (traverse-s-expr/recur d qqd f))]
       [`#(,d ...)
        (for/vector ([d (in-list d)])
          (traverse-s-expr/recur d qqd f))]
       [d d])]))

