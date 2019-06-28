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
[send->] (assert (= v4 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 v5 false ) ) )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (ite v7 v8 false ) ) )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (declare-fun v10 () Int )
[<-recv] success
[send->] (declare-fun v11 () Bytes )
[<-recv] success
[send->] (assert (= v11 (uint256->bytes v10 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bytes )
[<-recv] success
[send->] (assert (= v12 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (msg-cat v11 v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (assert (= v14 (digest v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Int )
[<-recv] success
[send->] (assert (= v15 v0 ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 v1 ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v18 ) )
[<-recv] success
[send->] (assert (= v18 v17 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (assert (= v20 (= v18 v19 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v20 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v3 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v22 ) )
[<-recv] success
[send->] (assert (= v22 v15 ) )
[<-recv] success
[send->] (declare-fun v23 () Bool )
[<-recv] success
[send->] (assert (= v23 (= v22 v15 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v23 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v23 )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (ite v24 v25 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v26 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (assert (= v27 v10 ) )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v2 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v29 ) )
[<-recv] success
[send->] (assert (= v29 0 ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v29 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v30 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v30 )
[<-recv] success
[send->] (declare-fun v31 () Bytes )
[<-recv] success
[send->] (assert (= v31 (uint256->bytes v27 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (msg-cat v31 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 (digest v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (= v14 v34 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v35 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v35 )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (ite v36 v37 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v38 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v38 )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (ite v39 v40 false ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (ite v42 v43 false ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v41 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Int )
[<-recv] success
[send->] (assert (= v46 (- 4 v21 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (+ v28 v46 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (mod v47 3 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (ite v44 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v41 2 v49 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v45 v48 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (<= 0 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (< v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v55 false true ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v59 true v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v60 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v60 )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v62 v63 false ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v61 false true ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v65 true v64 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (+ v68 v16 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v16 v72 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v67 v69 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v67 0 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v75 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v51 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v51 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
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
[send->] (assert (= v4 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 v5 false ) ) )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (ite v7 v8 false ) ) )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (declare-fun v10 () Int )
[<-recv] success
[send->] (declare-fun v11 () Bytes )
[<-recv] success
[send->] (assert (= v11 (uint256->bytes v10 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bytes )
[<-recv] success
[send->] (assert (= v12 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (msg-cat v11 v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (assert (= v14 (digest v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Int )
[<-recv] success
[send->] (assert (= v15 v0 ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 v1 ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v18 ) )
[<-recv] success
[send->] (assert (= v18 v17 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (assert (= v20 (= v18 v19 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v20 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v3 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v22 ) )
[<-recv] success
[send->] (assert (= v22 v15 ) )
[<-recv] success
[send->] (declare-fun v23 () Bool )
[<-recv] success
[send->] (assert (= v23 (= v22 v15 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v23 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v23 )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (ite v24 v25 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v26 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (assert (= v27 v10 ) )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v2 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v29 ) )
[<-recv] success
[send->] (assert (= v29 0 ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v29 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v30 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v30 )
[<-recv] success
[send->] (declare-fun v31 () Bytes )
[<-recv] success
[send->] (assert (= v31 (uint256->bytes v27 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (msg-cat v31 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 (digest v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (= v14 v34 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v35 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v35 )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (ite v36 v37 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v38 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v38 )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (ite v39 v40 false ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (ite v42 v43 false ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v41 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Int )
[<-recv] success
[send->] (assert (= v46 (- 4 v21 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (+ v28 v46 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (mod v47 3 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (ite v44 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v41 2 v49 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v45 v48 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (<= 0 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (< v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v55 false true ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v59 true v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v60 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v60 )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v62 v63 false ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v61 false true ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v65 true v64 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (+ v68 v16 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v16 v72 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v67 v69 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v67 0 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v75 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v51 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v51 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
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
[send->] (assert (= v4 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 v5 false ) ) )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (ite v7 v8 false ) ) )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (declare-fun v10 () Int )
[<-recv] success
[send->] (declare-fun v11 () Bytes )
[<-recv] success
[send->] (assert (= v11 (uint256->bytes v10 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bytes )
[<-recv] success
[send->] (assert (= v12 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (msg-cat v11 v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (assert (= v14 (digest v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Int )
[<-recv] success
[send->] (assert (= v15 v0 ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 v1 ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v18 ) )
[<-recv] success
[send->] (assert (= v18 v17 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (assert (= v20 (= v18 v19 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v20 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v3 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v22 ) )
[<-recv] success
[send->] (assert (= v22 v15 ) )
[<-recv] success
[send->] (declare-fun v23 () Bool )
[<-recv] success
[send->] (assert (= v23 (= v22 v15 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v23 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v23 )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (ite v24 v25 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v26 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (assert (= v27 v10 ) )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v2 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v29 ) )
[<-recv] success
[send->] (assert (= v29 0 ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v29 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v30 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v30 )
[<-recv] success
[send->] (declare-fun v31 () Bytes )
[<-recv] success
[send->] (assert (= v31 (uint256->bytes v27 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (msg-cat v31 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 (digest v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (= v14 v34 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v35 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v35 )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (ite v36 v37 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v38 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v38 )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (ite v39 v40 false ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (ite v42 v43 false ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v41 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Int )
[<-recv] success
[send->] (assert (= v46 (- 4 v21 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (+ v28 v46 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (mod v47 3 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (ite v44 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v41 2 v49 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v45 v48 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (<= 0 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (< v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v55 false true ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v59 true v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v60 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v60 )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v62 v63 false ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v61 false true ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v65 true v64 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (+ v68 v16 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v16 v72 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v67 v69 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v67 0 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v75 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v51 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v51 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
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
[send->] (declare-fun v10 () Int )
[<-recv] success
[send->] (declare-fun v11 () Bytes )
[<-recv] success
[send->] (declare-fun v12 () Bytes )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Int )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v18 ) )
[<-recv] success
[send->] (assert (<= 0 v18 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (assert (= v20 (= v18 v19 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v22 ) )
[<-recv] success
[send->] (assert (<= 0 v22 ) )
[<-recv] success
[send->] (declare-fun v23 () Bool )
[<-recv] success
[send->] (assert (= v23 (= v22 v15 ) ) )
[<-recv] success
[send->] (assert v23 )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (ite v24 v25 false ) ) )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v29 ) )
[<-recv] success
[send->] (assert (<= 0 v29 ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v29 0 ) ) )
[<-recv] success
[send->] (assert v30 )
[<-recv] success
[send->] (declare-fun v31 () Bytes )
[<-recv] success
[send->] (assert (= v31 (uint256->bytes v27 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (msg-cat v31 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 (digest v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (= v14 v34 ) ) )
[<-recv] success
[send->] (assert v35 )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (ite v36 v37 false ) ) )
[<-recv] success
[send->] (assert v38 )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (ite v39 v40 false ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (ite v42 v43 false ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v41 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Int )
[<-recv] success
[send->] (assert (= v46 (- 4 v21 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (+ v28 v46 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (mod v47 3 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (ite v44 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v41 2 v49 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v45 v48 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (<= 0 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (< v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v55 false true ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v59 true v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v60 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v60 )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v62 v63 false ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v61 false true ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v65 true v64 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (+ v68 v16 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v16 v72 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v67 v69 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v67 0 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v75 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v51 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v51 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
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
[send->] (assert (= v4 (<= 0 v2 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (< v2 3 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 v5 false ) ) )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (declare-fun v10 () Int )
[<-recv] success
[send->] (declare-fun v11 () Bytes )
[<-recv] success
[send->] (assert (= v11 (uint256->bytes v10 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bytes )
[<-recv] success
[send->] (assert (= v12 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (assert (= v13 (msg-cat v11 v12 ) ) )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (assert (= v14 (digest v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Int )
[<-recv] success
[send->] (assert (= v15 v0 ) )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (assert (= v16 v1 ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v18 ) )
[<-recv] success
[send->] (assert (<= 0 v18 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (assert (= v20 (= v18 v19 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v22 ) )
[<-recv] success
[send->] (assert (<= 0 v22 ) )
[<-recv] success
[send->] (declare-fun v23 () Bool )
[<-recv] success
[send->] (assert (= v23 (= v22 v15 ) ) )
[<-recv] success
[send->] (assert v23 )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (ite v24 v25 false ) ) )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (assert (= v27 v10 ) )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v2 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v29 ) )
[<-recv] success
[send->] (assert (<= 0 v29 ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v29 0 ) ) )
[<-recv] success
[send->] (assert v30 )
[<-recv] success
[send->] (declare-fun v31 () Bytes )
[<-recv] success
[send->] (assert (= v31 (uint256->bytes v27 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (msg-cat v31 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 (digest v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (= v14 v34 ) ) )
[<-recv] success
[send->] (assert v35 )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (ite v36 v37 false ) ) )
[<-recv] success
[send->] (assert v38 )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (ite v39 v40 false ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (ite v42 v43 false ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v41 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Int )
[<-recv] success
[send->] (assert (= v46 (- 4 v21 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (+ v28 v46 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (mod v47 3 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (ite v44 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v41 2 v49 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v45 v48 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (<= 0 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (< v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v55 false true ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v59 true v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v60 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v60 )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v62 v63 false ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v61 false true ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v65 true v64 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (+ v68 v16 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v16 v72 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v67 v69 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v67 0 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v75 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v51 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v51 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
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
[send->] (assert (= v7 (<= 0 v3 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (< v3 3 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (ite v7 v8 false ) ) )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (declare-fun v10 () Int )
[<-recv] success
[send->] (declare-fun v11 () Bytes )
[<-recv] success
[send->] (declare-fun v12 () Bytes )
[<-recv] success
[send->] (declare-fun v13 () Bytes )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Int )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v18 ) )
[<-recv] success
[send->] (assert (<= 0 v18 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (assert (= v20 (= v18 v19 ) ) )
[<-recv] success
[send->] (assert v20 )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v3 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v22 ) )
[<-recv] success
[send->] (assert (<= 0 v22 ) )
[<-recv] success
[send->] (declare-fun v23 () Bool )
[<-recv] success
[send->] (assert (= v23 (= v22 v15 ) ) )
[<-recv] success
[send->] (assert v23 )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (ite v24 v25 false ) ) )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v29 ) )
[<-recv] success
[send->] (assert (<= 0 v29 ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v29 0 ) ) )
[<-recv] success
[send->] (assert v30 )
[<-recv] success
[send->] (declare-fun v31 () Bytes )
[<-recv] success
[send->] (assert (= v31 (uint256->bytes v27 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (msg-cat v31 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 (digest v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (= v14 v34 ) ) )
[<-recv] success
[send->] (assert v35 )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (ite v36 v37 false ) ) )
[<-recv] success
[send->] (assert v38 )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (ite v39 v40 false ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (ite v42 v43 false ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v41 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Int )
[<-recv] success
[send->] (assert (= v46 (- 4 v21 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (+ v28 v46 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (mod v47 3 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (ite v44 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v41 2 v49 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v45 v48 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (<= 0 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (< v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (ite v52 v53 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v54 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v54 )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v28 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v28 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v55 false true ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v59 true v58 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v60 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v60 )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (<= 0 v21 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (< v21 3 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v62 v63 false ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v61 false true ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v65 true v64 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v66 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v66 )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v51 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (+ v68 v16 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v51 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v16 v72 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v67 v69 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v67 0 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v75 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v76 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v51 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v51 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v78 )
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
