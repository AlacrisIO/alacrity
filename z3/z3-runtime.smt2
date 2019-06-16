
; (=> (= (integer->integer-bytes a) (integer->integer-bytes b))
;     (= a b))

; (=> (= (digest a) (digest b))
;     (= a b))

; (=> (= (msg-cat a1 a2) (msg-cat b1 b2))
;     (&& (= a1 b1) (= a2 b2)))

(declare-datatypes ()
  ((Bytes
    (integer->integer-bytes (bytes-integer Int))
    (msg-cat (msg-left Bytes) (msg-right Bytes)))))

(declare-fun digest (Bytes) Int)

;; This definition only works in Z3 version >=4.8.5,
;; because `match` is broken in Z3 version <=4.8.4.
; (define-fun-rec bytes-length ((bs Bytes)) Int
;   (match bs
;     (((integer->integer-bytes _) 32)
;      ((digest _) 32)
;      ((msg-cat l r) (+ 32 (bytes-length l) (bytes-length r))))))

(define-fun-rec bytes-length ((bs Bytes)) Int
  (if ((_ is integer->integer-bytes) bs)
      32
      (if ((_ is digest) bs)
          32
          (+ 32 (bytes-length (msg-left bs)) (bytes-length (msg-right bs))))))
