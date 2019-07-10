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
[send->] (declare-fun v18 () Bytes )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (= v20 v19 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v15 v16 ) ) )
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
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v25 ) )
[<-recv] success
[send->] (assert (= v25 v15 ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (= v25 v15 ) ) )
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
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (ite v27 v28 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v29 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v29 )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v10 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (assert (= v31 v2 ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v33 ) )
[<-recv] success
[send->] (assert (= v33 0 ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v34 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bytes )
[<-recv] success
[send->] (assert (= v36 (uint256->bytes v31 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bytes )
[<-recv] success
[send->] (assert (= v37 (msg-cat v35 v36 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Int )
[<-recv] success
[send->] (assert (= v38 (digest v37 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (= v17 v38 ) ) )
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
[send->] (assert (= v40 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v42 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (ite v46 v47 false ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (ite v45 v48 false ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (+ v31 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (mod v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v48 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Int )
[<-recv] success
[send->] (assert (= v54 (ite v45 2 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Int )
[<-recv] success
[send->] (assert (= v55 (ite v49 v52 v54 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v55 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v58 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v58 )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 false true ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v63 true v62 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 false true ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (ite v69 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v70 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v70 )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v73 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v73 v74 v15 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (ite v71 v72 v75 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Int )
[<-recv] success
[send->] (assert (= v78 (ite v71 0 v76 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Int )
[<-recv] success
[send->] (assert (= v79 (+ v16 v77 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v79 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v78 ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v31 0 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v81 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v82 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (= v31 1 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v31 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v80 false ) ) )
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
[send->] (assert (= v87 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (= v23 0 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (ite v88 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (= v23 1 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (assert (= v91 (ite v90 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (assert (= v92 (= v23 2 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (assert (= v93 (ite v92 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v94 () Bytes )
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
[send->] (declare-fun v18 () Bytes )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (= v20 v19 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v15 v16 ) ) )
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
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v25 ) )
[<-recv] success
[send->] (assert (= v25 v15 ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (= v25 v15 ) ) )
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
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (ite v27 v28 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v29 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v29 )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v10 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (assert (= v31 v2 ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v33 ) )
[<-recv] success
[send->] (assert (= v33 0 ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v34 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bytes )
[<-recv] success
[send->] (assert (= v36 (uint256->bytes v31 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bytes )
[<-recv] success
[send->] (assert (= v37 (msg-cat v35 v36 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Int )
[<-recv] success
[send->] (assert (= v38 (digest v37 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (= v17 v38 ) ) )
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
[send->] (assert (= v40 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v42 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (ite v46 v47 false ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (ite v45 v48 false ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (+ v31 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (mod v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v48 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Int )
[<-recv] success
[send->] (assert (= v54 (ite v45 2 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Int )
[<-recv] success
[send->] (assert (= v55 (ite v49 v52 v54 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v55 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v58 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v58 )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 false true ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v63 true v62 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 false true ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (ite v69 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v70 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v70 )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v73 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v73 v74 v15 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (ite v71 v72 v75 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Int )
[<-recv] success
[send->] (assert (= v78 (ite v71 0 v76 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Int )
[<-recv] success
[send->] (assert (= v79 (+ v16 v77 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v79 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v78 ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v31 0 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v81 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v82 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (= v31 1 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v31 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v80 false ) ) )
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
[send->] (assert (= v87 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (= v23 0 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (ite v88 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (= v23 1 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (assert (= v91 (ite v90 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (assert (= v92 (= v23 2 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (assert (= v93 (ite v92 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v94 () Bytes )
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
[send->] (declare-fun v18 () Bytes )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (= v20 v19 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v15 v16 ) ) )
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
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v25 ) )
[<-recv] success
[send->] (assert (= v25 v15 ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (= v25 v15 ) ) )
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
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (ite v27 v28 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v29 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v29 )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v10 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (assert (= v31 v2 ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v33 ) )
[<-recv] success
[send->] (assert (= v33 0 ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v34 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bytes )
[<-recv] success
[send->] (assert (= v36 (uint256->bytes v31 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bytes )
[<-recv] success
[send->] (assert (= v37 (msg-cat v35 v36 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Int )
[<-recv] success
[send->] (assert (= v38 (digest v37 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (= v17 v38 ) ) )
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
[send->] (assert (= v40 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v42 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (ite v46 v47 false ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (ite v45 v48 false ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (+ v31 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (mod v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v48 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Int )
[<-recv] success
[send->] (assert (= v54 (ite v45 2 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Int )
[<-recv] success
[send->] (assert (= v55 (ite v49 v52 v54 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v55 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v58 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v58 )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 false true ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v63 true v62 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 false true ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (ite v69 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v70 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v70 )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v73 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v73 v74 v15 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (ite v71 v72 v75 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Int )
[<-recv] success
[send->] (assert (= v78 (ite v71 0 v76 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Int )
[<-recv] success
[send->] (assert (= v79 (+ v16 v77 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v79 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v78 ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v31 0 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v81 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v82 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (= v31 1 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v31 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v80 false ) ) )
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
[send->] (assert (= v87 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (= v23 0 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (ite v88 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (= v23 1 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (assert (= v91 (ite v90 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (assert (= v92 (= v23 2 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (assert (= v93 (ite v92 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v94 () Bytes )
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
[send->] (declare-fun v18 () Bytes )
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
[send->] (assert (= v21 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v25 ) )
[<-recv] success
[send->] (assert (<= 0 v25 ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (= v25 v15 ) ) )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (ite v27 v28 false ) ) )
[<-recv] success
[send->] (assert v29 )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v33 ) )
[<-recv] success
[send->] (assert (<= 0 v33 ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bytes )
[<-recv] success
[send->] (assert (= v36 (uint256->bytes v31 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bytes )
[<-recv] success
[send->] (assert (= v37 (msg-cat v35 v36 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Int )
[<-recv] success
[send->] (assert (= v38 (digest v37 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (= v17 v38 ) ) )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (ite v46 v47 false ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (ite v45 v48 false ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (+ v31 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (mod v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v48 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Int )
[<-recv] success
[send->] (assert (= v54 (ite v45 2 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Int )
[<-recv] success
[send->] (assert (= v55 (ite v49 v52 v54 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v55 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v58 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v58 )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 false true ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v63 true v62 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 false true ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (ite v69 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v70 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v70 )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v73 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v73 v74 v15 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (ite v71 v72 v75 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Int )
[<-recv] success
[send->] (assert (= v78 (ite v71 0 v76 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Int )
[<-recv] success
[send->] (assert (= v79 (+ v16 v77 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v79 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v78 ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v31 0 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v81 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v82 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (= v31 1 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v31 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v80 false ) ) )
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
[send->] (assert (= v87 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (= v23 0 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (ite v88 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (= v23 1 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (assert (= v91 (ite v90 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (assert (= v92 (= v23 2 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (assert (= v93 (ite v92 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v94 () Bytes )
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
[send->] (declare-fun v18 () Bytes )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v20 ) )
[<-recv] success
[send->] (assert (<= 0 v20 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 (+ v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v22 () Bool )
[<-recv] success
[send->] (assert (= v22 (= v20 v21 ) ) )
[<-recv] success
[send->] (assert v22 )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v25 ) )
[<-recv] success
[send->] (assert (<= 0 v25 ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (= v25 v15 ) ) )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (ite v27 v28 false ) ) )
[<-recv] success
[send->] (assert v29 )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (assert (= v30 v10 ) )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (assert (= v31 v2 ) )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v33 ) )
[<-recv] success
[send->] (assert (<= 0 v33 ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bytes )
[<-recv] success
[send->] (assert (= v36 (uint256->bytes v31 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bytes )
[<-recv] success
[send->] (assert (= v37 (msg-cat v35 v36 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Int )
[<-recv] success
[send->] (assert (= v38 (digest v37 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (= v17 v38 ) ) )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (ite v46 v47 false ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (ite v45 v48 false ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (+ v31 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (mod v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v48 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Int )
[<-recv] success
[send->] (assert (= v54 (ite v45 2 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Int )
[<-recv] success
[send->] (assert (= v55 (ite v49 v52 v54 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v55 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v58 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v58 )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 false true ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v63 true v62 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 false true ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (ite v69 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v70 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v70 )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v73 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v73 v74 v15 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (ite v71 v72 v75 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Int )
[<-recv] success
[send->] (assert (= v78 (ite v71 0 v76 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Int )
[<-recv] success
[send->] (assert (= v79 (+ v16 v77 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v79 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v78 ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v31 0 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v81 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v82 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (= v31 1 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v31 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v80 false ) ) )
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
[send->] (assert (= v87 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (= v23 0 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (ite v88 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (= v23 1 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (assert (= v91 (ite v90 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (assert (= v92 (= v23 2 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (assert (= v93 (ite v92 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v94 () Bytes )
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
[send->] (declare-fun v18 () Bytes )
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
[send->] (assert (= v21 (+ v15 v16 ) ) )
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
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v25 ) )
[<-recv] success
[send->] (assert (<= 0 v25 ) )
[<-recv] success
[send->] (declare-fun v26 () Bool )
[<-recv] success
[send->] (assert (= v26 (= v25 v15 ) ) )
[<-recv] success
[send->] (assert v26 )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (ite v27 v28 false ) ) )
[<-recv] success
[send->] (assert v29 )
[<-recv] success
[send->] (declare-fun v30 () Int )
[<-recv] success
[send->] (declare-fun v31 () Int )
[<-recv] success
[send->] (declare-fun v32 () Bytes )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v33 ) )
[<-recv] success
[send->] (assert (<= 0 v33 ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (= v33 0 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (declare-fun v35 () Bytes )
[<-recv] success
[send->] (assert (= v35 (uint256->bytes v30 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bytes )
[<-recv] success
[send->] (assert (= v36 (uint256->bytes v31 ) ) )
[<-recv] success
[send->] (declare-fun v37 () Bytes )
[<-recv] success
[send->] (assert (= v37 (msg-cat v35 v36 ) ) )
[<-recv] success
[send->] (declare-fun v38 () Int )
[<-recv] success
[send->] (assert (= v38 (digest v37 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (assert (= v39 (= v17 v38 ) ) )
[<-recv] success
[send->] (assert v39 )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (ite v40 v41 false ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (ite v46 v47 false ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (ite v45 v48 false ) ) )
[<-recv] success
[send->] (declare-fun v50 () Int )
[<-recv] success
[send->] (assert (= v50 (- 4 v23 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Int )
[<-recv] success
[send->] (assert (= v51 (+ v31 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (mod v51 3 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Int )
[<-recv] success
[send->] (assert (= v53 (ite v48 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Int )
[<-recv] success
[send->] (assert (= v54 (ite v45 2 v53 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Int )
[<-recv] success
[send->] (assert (= v55 (ite v49 v52 v54 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (<= 0 v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (< v55 3 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v56 v57 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v58 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v58 )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v31 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v31 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 false true ) ) )
[<-recv] success
[send->] (declare-fun v64 () Bool )
[<-recv] success
[send->] (assert (= v64 (ite v63 true v62 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v64 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v64 )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (<= 0 v23 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (< v23 3 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 v67 false ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 false true ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (ite v69 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v70 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v70 )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Int )
[<-recv] success
[send->] (assert (= v72 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Int )
[<-recv] success
[send->] (assert (= v74 (* 2 v15 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Int )
[<-recv] success
[send->] (assert (= v75 (ite v73 0 v15 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Int )
[<-recv] success
[send->] (assert (= v76 (ite v73 v74 v15 ) ) )
[<-recv] success
[send->] (declare-fun v77 () Int )
[<-recv] success
[send->] (assert (= v77 (ite v71 v72 v75 ) ) )
[<-recv] success
[send->] (declare-fun v78 () Int )
[<-recv] success
[send->] (assert (= v78 (ite v71 0 v76 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Int )
[<-recv] success
[send->] (assert (= v79 (+ v16 v77 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v79 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v78 ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v55 2 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v31 0 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v81 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v82 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (= v31 1 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 v80 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v31 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (ite v85 v80 false ) ) )
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
[send->] (assert (= v87 (= v55 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (assert (= v88 (= v23 0 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (ite v88 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (assert (= v90 (= v23 1 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (assert (= v91 (ite v90 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (assert (= v92 (= v23 2 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (assert (= v93 (ite v92 v87 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v93 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v94 () Bytes )
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
