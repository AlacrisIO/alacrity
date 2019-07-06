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
[send->] (assert (= v17 v14 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v19 ) )
[<-recv] success
[send->] (assert (= v19 v18 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v21 () Bool )
[<-recv] success
[send->] (assert (= v21 (= v19 v20 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v21 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v21 )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 v3 ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v23 ) )
[<-recv] success
[send->] (assert (= v23 v15 ) )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (= v23 v15 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v24 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v24 )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (ite v25 v26 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v27 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v27 )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v10 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v2 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v30 ) )
[<-recv] success
[send->] (assert (= v30 0 ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v31 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (msg-cat v32 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 (digest v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (= v17 v35 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v36 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v36 )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (ite v37 v38 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v39 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v42 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (- 4 v22 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (+ v29 v47 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (mod v48 3 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v45 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v42 2 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v46 v49 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (<= 0 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (< v52 3 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (ite v53 v54 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v55 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v55 )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v56 false true ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v60 true v59 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v62 false true ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v66 true v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v67 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v67 )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (ite v70 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v68 v69 v72 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v68 0 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (+ v16 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v76 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v75 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v29 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (ite v78 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v29 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (ite v80 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v81 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (= v29 2 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v82 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v83 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v22 0 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v86 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v22 1 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (ite v87 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v22 2 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (ite v89 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v90 )
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
[send->] (assert (= v17 v14 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v19 ) )
[<-recv] success
[send->] (assert (= v19 v18 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v21 () Bool )
[<-recv] success
[send->] (assert (= v21 (= v19 v20 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v21 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v21 )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 v3 ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v23 ) )
[<-recv] success
[send->] (assert (= v23 v15 ) )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (= v23 v15 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v24 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v24 )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (ite v25 v26 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v27 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v27 )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v10 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v2 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v30 ) )
[<-recv] success
[send->] (assert (= v30 0 ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v31 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (msg-cat v32 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 (digest v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (= v17 v35 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v36 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v36 )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (ite v37 v38 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v39 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v42 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (- 4 v22 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (+ v29 v47 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (mod v48 3 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v45 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v42 2 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v46 v49 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (<= 0 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (< v52 3 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (ite v53 v54 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v55 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v55 )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v56 false true ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v60 true v59 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v62 false true ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v66 true v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v67 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v67 )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (ite v70 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v68 v69 v72 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v68 0 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (+ v16 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v76 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v75 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v29 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (ite v78 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v29 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (ite v80 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v81 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (= v29 2 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v82 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v83 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v22 0 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v86 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v22 1 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (ite v87 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v22 2 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (ite v89 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v90 )
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
[send->] (assert (= v17 v14 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v19 ) )
[<-recv] success
[send->] (assert (= v19 v18 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v21 () Bool )
[<-recv] success
[send->] (assert (= v21 (= v19 v20 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v21 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v21 )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 v3 ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v23 ) )
[<-recv] success
[send->] (assert (= v23 v15 ) )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (= v23 v15 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v24 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v24 )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (ite v25 v26 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v27 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v27 )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v10 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v2 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v30 ) )
[<-recv] success
[send->] (assert (= v30 0 ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v31 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (msg-cat v32 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 (digest v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (= v17 v35 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v36 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v36 )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (ite v37 v38 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v39 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v42 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (- 4 v22 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (+ v29 v47 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (mod v48 3 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v45 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v42 2 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v46 v49 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (<= 0 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (< v52 3 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (ite v53 v54 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v55 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v55 )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v56 false true ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v60 true v59 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v62 false true ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v66 true v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v67 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v67 )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (ite v70 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v68 v69 v72 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v68 0 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (+ v16 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v76 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v75 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v29 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (ite v78 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v29 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (ite v80 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v81 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (= v29 2 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v82 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v83 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v22 0 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v86 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v22 1 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (ite v87 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v22 2 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (ite v89 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v90 )
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
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v19 ) )
[<-recv] success
[send->] (assert (<= 0 v19 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v21 () Bool )
[<-recv] success
[send->] (assert (= v21 (= v19 v20 ) ) )
[<-recv] success
[send->] (assert v21 )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v23 ) )
[<-recv] success
[send->] (assert (<= 0 v23 ) )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (= v23 v15 ) ) )
[<-recv] success
[send->] (assert v24 )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (ite v25 v26 false ) ) )
[<-recv] success
[send->] (assert v27 )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v30 ) )
[<-recv] success
[send->] (assert (<= 0 v30 ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (msg-cat v32 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 (digest v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (= v17 v35 ) ) )
[<-recv] success
[send->] (assert v36 )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (ite v37 v38 false ) ) )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v42 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (- 4 v22 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (+ v29 v47 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (mod v48 3 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v45 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v42 2 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v46 v49 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (<= 0 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (< v52 3 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (ite v53 v54 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v55 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v55 )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v56 false true ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v60 true v59 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v62 false true ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v66 true v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v67 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v67 )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (ite v70 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v68 v69 v72 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v68 0 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (+ v16 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v76 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v75 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v29 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (ite v78 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v29 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (ite v80 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v81 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (= v29 2 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v82 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v83 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v22 0 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v86 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v22 1 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (ite v87 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v22 2 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (ite v89 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v90 )
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
[send->] (assert (= v17 v14 ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v19 ) )
[<-recv] success
[send->] (assert (<= 0 v19 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v21 () Bool )
[<-recv] success
[send->] (assert (= v21 (= v19 v20 ) ) )
[<-recv] success
[send->] (assert v21 )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v23 ) )
[<-recv] success
[send->] (assert (<= 0 v23 ) )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (= v23 v15 ) ) )
[<-recv] success
[send->] (assert v24 )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (ite v25 v26 false ) ) )
[<-recv] success
[send->] (assert v27 )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (assert (= v28 v10 ) )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (assert (= v29 v2 ) )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v30 ) )
[<-recv] success
[send->] (assert (<= 0 v30 ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (msg-cat v32 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 (digest v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (= v17 v35 ) ) )
[<-recv] success
[send->] (assert v36 )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (ite v37 v38 false ) ) )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v42 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (- 4 v22 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (+ v29 v47 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (mod v48 3 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v45 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v42 2 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v46 v49 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (<= 0 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (< v52 3 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (ite v53 v54 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v55 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v55 )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v56 false true ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v60 true v59 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v62 false true ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v66 true v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v67 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v67 )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (ite v70 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v68 v69 v72 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v68 0 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (+ v16 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v76 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v75 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v29 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (ite v78 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v29 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (ite v80 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v81 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (= v29 2 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v82 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v83 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v22 0 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v86 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v22 1 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (ite v87 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v22 2 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (ite v89 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v90 )
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
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v19 ) )
[<-recv] success
[send->] (assert (<= 0 v19 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v21 () Bool )
[<-recv] success
[send->] (assert (= v21 (= v19 v20 ) ) )
[<-recv] success
[send->] (assert v21 )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 v3 ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v23 ) )
[<-recv] success
[send->] (assert (<= 0 v23 ) )
[<-recv] success
[send->] (declare-fun v24 () Bool )
[<-recv] success
[send->] (assert (= v24 (= v23 v15 ) ) )
[<-recv] success
[send->] (assert v24 )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (ite v25 v26 false ) ) )
[<-recv] success
[send->] (assert v27 )
[<-recv] success
[send->] (declare-fun v28 () Int )
[<-recv] success
[send->] (declare-fun v29 () Int )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v30 ) )
[<-recv] success
[send->] (assert (<= 0 v30 ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v30 0 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (assert (= v32 (uint256->bytes v28 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bytes )
[<-recv] success
[send->] (assert (= v33 (uint256->bytes v29 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bytes )
[<-recv] success
[send->] (assert (= v34 (msg-cat v32 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 (digest v34 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (= v17 v35 ) ) )
[<-recv] success
[send->] (assert v36 )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (assert (= v38 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (ite v37 v38 false ) ) )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v42 v45 false ) ) )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (assert (= v47 (- 4 v22 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Int )
[<-recv] success
[send->] (assert (= v48 (+ v29 v47 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Int )
[<-recv] success
[send->] (assert (= v49 (mod v48 3 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (ite v45 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (ite v42 2 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (ite v46 v49 v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (<= 0 v52 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (< v52 3 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (ite v53 v54 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v55 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v55 )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v29 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v29 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (ite v56 false true ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (ite v60 true v59 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v61 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v61 )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (<= 0 v22 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (< v22 3 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (ite v63 v64 false ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (ite v62 false true ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (ite v66 true v65 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v67 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v67 )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Int )
[<-recv] success
[send->] (assert (= v71 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (ite v70 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Int )
[<-recv] success
[send->] (assert (= v73 (ite v70 v71 v15 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (ite v68 v69 v72 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v68 0 v73 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (+ v16 v74 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v76 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v75 ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (= v52 2 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v29 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (ite v78 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v79 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v29 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (ite v80 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v81 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (= v29 2 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v82 v77 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v83 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (= v52 0 ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v22 0 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v86 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v22 1 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (ite v87 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v88 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v22 2 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (ite v89 v84 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v90 )
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
