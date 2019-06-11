
; (=> (= (integer->bytes a) (integer->bytes b))
;     (= a b))

; (=> (= (digest a) (digest b))
;     (= a b))

; (=> (= (msg-cat a1 a2) (msg-cat b1 b2))
;     (&& (= a1 b1) (= a2 b2)))

(declare-datatypes ()
  ((Bytes
    (integer->bytes (bytes-integer Int))
    (digest (digest-value Bytes))
    (msg-cat (msg-left Bytes) (msg-right Bytes)))))

(define-fun-rec bytes-length ((bs Bytes)) Int
  (match bs
    (((integer->bytes _) 32)
     ((digest _) 32)
     ((msg-cat l r) (+ 32 (bytes-length l) (bytes-length r))))))

