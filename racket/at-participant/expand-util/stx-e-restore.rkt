#lang racket/base

(provide stx-e
         restore)

;; stx-e : Stx -> E
(define (stx-e stx)
  (if (syntax? stx) (syntax-e stx) stx))

;; restore : Stx E -> Stx
(define (restore stx e)
  (if (syntax? stx)
      (let ([stx* (syntax-disarm stx #f)])
        (syntax-rearm (datum->syntax stx* e stx* stx*) stx))
      e))

