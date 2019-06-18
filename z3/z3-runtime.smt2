(declare-datatypes ()
  ((Bytes
    (uint256->bytes (bytes-integer Int))
    (msg-cat (msg-left Bytes) (msg-right Bytes)))))

(declare-fun digest (Bytes) Int)

;; This definition only works in Z3 version >=4.8.5,
;; because `match` is broken in Z3 version <=4.8.4.
; (define-fun-rec bytes-length ((bs Bytes)) Int
;   (match bs
;     (((uint256->bytes _) 32)
;      ((digest _) 32)
;      ((msg-cat l r) (+ 32 (bytes-length l) (bytes-length r))))))

(define-fun-rec bytes-length ((bs Bytes)) Int
  (if ((_ is uint256->bytes) bs)
      32
      (if ((_ is digest) bs)
          32
          (+ 32 (bytes-length (msg-left bs)) (bytes-length (msg-right bs))))))
