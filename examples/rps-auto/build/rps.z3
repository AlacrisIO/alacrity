[send->] (set-option :print-success true )
[<-recv] success
[send->] (set-option :produce-models true )
[<-recv] success
[send->] (set-option :produce-unsat-cores true )
[<-recv] success
[send->] (declare-datatypes () ((Bytes (uint256->bytes (bytes-integer Int ) ) (msg-cat (msg-left Bytes ) (msg-right Bytes ) ) ) ) )
[<-recv] success
[send->] (declare-fun digest (Bytes ) Int )
[<-recv] success
[send->] (define-fun-rec bytes-length ((bs Bytes ) ) Int (if ((_ is uint256->bytes ) bs ) 32 (if ((_ is digest ) bs ) 32 (+ 32 (bytes-length (msg-left bs ) ) (bytes-length (msg-right bs ) ) ) ) ) )
[<-recv] success
[send->] (declare-fun v0 () Int )
[<-recv] success
[send->] (declare-fun v1 () Int )
[<-recv] success
[send->] (declare-fun v2 () Int )
[<-recv] success
[send->] (declare-fun v3 () Int )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (> v1 0 ) ) )
[<-recv] success
[send->] (assert v4 )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (> v0 0 ) ) )
[<-recv] success
[send->] (assert v5 )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v6 v7 false ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (ite v9 v10 false ) ) )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (declare-fun v12 () Int )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (uint256->bytes v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (msg-cat v13 v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 (digest v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 v0 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v1 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (= v20 v19 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v22 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (assert (= v23 v3 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v24 ) )
[<-recv] success
[send->] (assert (= v24 v17 ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v24 v17 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (ite v26 v27 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v28 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v12 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v2 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v31 ) )
[<-recv] success
[send->] (assert (= v31 0 ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (= v31 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v32 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v32 )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (msg-cat v33 v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (assert (= v36 (digest v35 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v16 v36 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v37 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (ite v38 v39 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v40 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 v46 false ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (+ v30 v48 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (mod v49 3 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v46 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v43 2 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v47 v50 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v53 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v56 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v58 v59 false ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v57 false true ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v61 true v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v64 v65 false ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v63 false true ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v67 true v66 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v68 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v68 )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Int )
[<-recv] success
[send->] (assert (= v70 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v71 0 v17 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v71 v72 v17 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v69 v70 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v69 0 v74 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (+ v18 v75 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v77 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v53 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (> v1 0 ) ) )
[<-recv] success
[send->] (assert v4 )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (> v0 0 ) ) )
[<-recv] success
[send->] (assert v5 )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v6 v7 false ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (ite v9 v10 false ) ) )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (declare-fun v12 () Int )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (uint256->bytes v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (msg-cat v13 v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 (digest v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 v0 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v1 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (= v20 v19 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v22 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (assert (= v23 v3 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v24 ) )
[<-recv] success
[send->] (assert (= v24 v17 ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v24 v17 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (ite v26 v27 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v28 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v12 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v2 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v31 ) )
[<-recv] success
[send->] (assert (= v31 0 ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (= v31 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v32 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v32 )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (msg-cat v33 v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (assert (= v36 (digest v35 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v16 v36 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v37 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (ite v38 v39 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v40 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 v46 false ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (+ v30 v48 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (mod v49 3 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v46 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v43 2 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v47 v50 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v53 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v56 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v58 v59 false ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v57 false true ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v61 true v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v64 v65 false ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v63 false true ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v67 true v66 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v68 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v68 )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Int )
[<-recv] success
[send->] (assert (= v70 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v71 0 v17 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v71 v72 v17 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v69 v70 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v69 0 v74 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (+ v18 v75 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v77 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v53 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (> v1 0 ) ) )
[<-recv] success
[send->] (assert v4 )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (> v0 0 ) ) )
[<-recv] success
[send->] (assert v5 )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v6 v7 false ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (ite v9 v10 false ) ) )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (declare-fun v12 () Int )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (uint256->bytes v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (msg-cat v13 v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 (digest v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 v0 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v1 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (= v20 v19 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v22 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (assert (= v23 v3 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v24 ) )
[<-recv] success
[send->] (assert (= v24 v17 ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v24 v17 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v25 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (ite v26 v27 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v28 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v12 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v2 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v31 ) )
[<-recv] success
[send->] (assert (= v31 0 ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (= v31 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v32 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v32 )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (msg-cat v33 v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (assert (= v36 (digest v35 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v16 v36 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v37 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (ite v38 v39 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v40 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 v46 false ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (+ v30 v48 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (mod v49 3 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v46 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v43 2 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v47 v50 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v53 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v56 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v58 v59 false ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v57 false true ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v61 true v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v64 v65 false ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v63 false true ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v67 true v66 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v68 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v68 )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Int )
[<-recv] success
[send->] (assert (= v70 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v71 0 v17 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v71 v72 v17 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v69 v70 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v69 0 v74 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (+ v18 v75 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v77 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v53 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v12 () Int )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (<= 0 v20 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v24 ) )
[<-recv] success
[send->] (assert (<= 0 v24 ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v24 v17 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (ite v26 v27 false ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v31 ) )
[<-recv] success
[send->] (assert (<= 0 v31 ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (= v31 0 ) ) )
[<-recv] success
[send->] (assert v32 )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (msg-cat v33 v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (assert (= v36 (digest v35 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v16 v36 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (ite v38 v39 false ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 v46 false ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (+ v30 v48 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (mod v49 3 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v46 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v43 2 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v47 v50 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v53 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v56 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v58 v59 false ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v57 false true ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v61 true v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v64 v65 false ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v63 false true ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v67 true v66 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v68 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v68 )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Int )
[<-recv] success
[send->] (assert (= v70 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v71 0 v17 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v71 v72 v17 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v69 v70 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v69 0 v74 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (+ v18 v75 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v77 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v53 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (> v1 0 ) ) )
[<-recv] success
[send->] (assert v4 )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (> v0 0 ) ) )
[<-recv] success
[send->] (assert v5 )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v6 v7 false ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v12 () Int )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (uint256->bytes v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (msg-cat v13 v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 (digest v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 v0 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v1 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (<= 0 v20 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v24 ) )
[<-recv] success
[send->] (assert (<= 0 v24 ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v24 v17 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (ite v26 v27 false ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v12 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v2 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v31 ) )
[<-recv] success
[send->] (assert (<= 0 v31 ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (= v31 0 ) ) )
[<-recv] success
[send->] (assert v32 )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (msg-cat v33 v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (assert (= v36 (digest v35 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v16 v36 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (ite v38 v39 false ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 v46 false ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (+ v30 v48 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (mod v49 3 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v46 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v43 2 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v47 v50 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v53 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v56 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v58 v59 false ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v57 false true ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v61 true v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v64 v65 false ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v63 false true ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v67 true v66 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v68 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v68 )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Int )
[<-recv] success
[send->] (assert (= v70 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v71 0 v17 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v71 v72 v17 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v69 v70 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v69 0 v74 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (+ v18 v75 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v77 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v53 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (ite v9 v10 false ) ) )
[<-recv] success
[send->] (assert v11 )
[<-recv] success
[send->] (declare-fun v12 () Int )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (<= 0 v20 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v17 v18 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (assert (= v23 v3 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v24 ) )
[<-recv] success
[send->] (assert (<= 0 v24 ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v24 v17 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (ite v26 v27 false ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v31 ) )
[<-recv] success
[send->] (assert (<= 0 v31 ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (= v31 0 ) ) )
[<-recv] success
[send->] (assert v32 )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (msg-cat v33 v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (assert (= v36 (digest v35 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v16 v36 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (ite v38 v39 false ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 v46 false ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (+ v30 v48 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (mod v49 3 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v46 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v43 2 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v47 v50 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v53 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v56 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (<= 0 v30 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (< v30 3 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v58 v59 false ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v57 false true ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v61 true v60 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v62 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v62 )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v64 v65 false ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v63 false true ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v67 true v66 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v68 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v68 )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (= v53 2 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Int )
[<-recv] success
[send->] (assert (= v70 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v53 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v17 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v71 0 v17 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v71 v72 v17 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v69 v70 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v69 0 v74 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (+ v18 v75 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v77 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v53 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v53 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance5 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (exit )
