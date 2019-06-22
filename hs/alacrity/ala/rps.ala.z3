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
[stderr] ; define-fun-rec
[<-recv] unsupported
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
[send->] (assert (= v4 (= v2 0 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 1 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (= v2 2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v5 true v6 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v4 true v7 ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (= v3 0 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (= v3 1 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (= v3 2 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 true v11 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (assert (= v13 (ite v9 true v12 ) ) )
[<-recv] success
[send->] (assert v13 )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Bytes )
[<-recv] success
[send->] (assert (= v17 (msg-cat v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (digest v17 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v0 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v1 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v18 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (= v23 v22 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
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
[send->] (declare-fun v26 () Int )
[<-recv] success
[send->] (assert (= v26 v3 ) )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v27 ) )
[<-recv] success
[send->] (assert (= v27 v19 ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v27 v19 ) ) )
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
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (ite v30 true v31 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (assert (= v33 (ite v29 true v32 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v33 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v33 )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 v14 ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 v2 ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v36 ) )
[<-recv] success
[send->] (assert (= v36 0 ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v36 0 ) ) )
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
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (assert (= v38 (uint256->bytes v34 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bytes )
[<-recv] success
[send->] (assert (= v39 (uint256->bytes v35 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bytes )
[<-recv] success
[send->] (assert (= v40 (msg-cat v38 v39 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (assert (= v41 (digest v40 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (= v21 v41 ) ) )
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
[send->] (assert (= v43 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 true v45 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 true v46 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v47 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v47 )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bool )
[<-recv] success
[send->] (assert (= v50 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bool )
[<-recv] success
[send->] (assert (= v51 (ite v49 true v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (ite v48 true v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 true v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (ite v53 true v56 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v52 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Int )
[<-recv] success
[send->] (assert (= v59 (- 4 v26 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Int )
[<-recv] success
[send->] (assert (= v60 (+ v35 v59 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Int )
[<-recv] success
[send->] (assert (= v61 (mod v60 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Int )
[<-recv] success
[send->] (assert (= v62 (ite v57 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Int )
[<-recv] success
[send->] (assert (= v63 (ite v52 2 v62 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (ite v58 v61 v63 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (= v64 1 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 true v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (ite v72 true v73 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (ite v71 true v74 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v70 false true ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v76 true v75 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v77 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 true v81 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 true v82 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v78 false true ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (ite v84 true v83 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Int )
[<-recv] success
[send->] (assert (= v87 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (+ v87 v20 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v89 v20 v91 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (ite v89 v90 v19 ) ) )
[<-recv] success
[send->] (declare-fun v94 () Int )
[<-recv] success
[send->] (assert (= v94 (ite v86 v88 v92 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Int )
[<-recv] success
[send->] (assert (= v95 (ite v86 0 v93 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v94 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v95 ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (= v64 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (assert (= v97 (= v64 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
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
[send->] (assert (= v4 (= v2 0 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 1 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (= v2 2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v5 true v6 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v4 true v7 ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (= v3 0 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (= v3 1 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (= v3 2 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 true v11 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (assert (= v13 (ite v9 true v12 ) ) )
[<-recv] success
[send->] (assert v13 )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Bytes )
[<-recv] success
[send->] (assert (= v17 (msg-cat v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (digest v17 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v0 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v1 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v18 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (= v23 v22 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
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
[send->] (declare-fun v26 () Int )
[<-recv] success
[send->] (assert (= v26 v3 ) )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v27 ) )
[<-recv] success
[send->] (assert (= v27 v19 ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v27 v19 ) ) )
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
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (ite v30 true v31 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (assert (= v33 (ite v29 true v32 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v33 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v33 )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 v14 ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 v2 ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v36 ) )
[<-recv] success
[send->] (assert (= v36 0 ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v36 0 ) ) )
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
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (assert (= v38 (uint256->bytes v34 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bytes )
[<-recv] success
[send->] (assert (= v39 (uint256->bytes v35 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bytes )
[<-recv] success
[send->] (assert (= v40 (msg-cat v38 v39 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (assert (= v41 (digest v40 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (= v21 v41 ) ) )
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
[send->] (assert (= v43 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 true v45 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 true v46 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v47 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v47 )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bool )
[<-recv] success
[send->] (assert (= v50 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bool )
[<-recv] success
[send->] (assert (= v51 (ite v49 true v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (ite v48 true v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 true v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (ite v53 true v56 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v52 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Int )
[<-recv] success
[send->] (assert (= v59 (- 4 v26 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Int )
[<-recv] success
[send->] (assert (= v60 (+ v35 v59 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Int )
[<-recv] success
[send->] (assert (= v61 (mod v60 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Int )
[<-recv] success
[send->] (assert (= v62 (ite v57 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Int )
[<-recv] success
[send->] (assert (= v63 (ite v52 2 v62 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (ite v58 v61 v63 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (= v64 1 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 true v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (ite v72 true v73 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (ite v71 true v74 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v70 false true ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v76 true v75 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v77 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 true v81 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 true v82 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v78 false true ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (ite v84 true v83 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Int )
[<-recv] success
[send->] (assert (= v87 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (+ v87 v20 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v89 v20 v91 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (ite v89 v90 v19 ) ) )
[<-recv] success
[send->] (declare-fun v94 () Int )
[<-recv] success
[send->] (assert (= v94 (ite v86 v88 v92 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Int )
[<-recv] success
[send->] (assert (= v95 (ite v86 0 v93 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v94 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v95 ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (= v64 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (assert (= v97 (= v64 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
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
[send->] (assert (= v4 (= v2 0 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 1 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (= v2 2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v5 true v6 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v4 true v7 ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (assert (= v9 (= v3 0 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (= v3 1 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (= v3 2 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 true v11 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (assert (= v13 (ite v9 true v12 ) ) )
[<-recv] success
[send->] (assert v13 )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Bytes )
[<-recv] success
[send->] (assert (= v17 (msg-cat v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (digest v17 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v0 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v1 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v18 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (= v23 v22 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
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
[send->] (declare-fun v26 () Int )
[<-recv] success
[send->] (assert (= v26 v3 ) )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v27 ) )
[<-recv] success
[send->] (assert (= v27 v19 ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v27 v19 ) ) )
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
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (ite v30 true v31 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (assert (= v33 (ite v29 true v32 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v33 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v33 )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 v14 ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 v2 ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v36 ) )
[<-recv] success
[send->] (assert (= v36 0 ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v36 0 ) ) )
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
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (assert (= v38 (uint256->bytes v34 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bytes )
[<-recv] success
[send->] (assert (= v39 (uint256->bytes v35 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bytes )
[<-recv] success
[send->] (assert (= v40 (msg-cat v38 v39 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (assert (= v41 (digest v40 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (= v21 v41 ) ) )
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
[send->] (assert (= v43 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 true v45 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 true v46 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v47 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v47 )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bool )
[<-recv] success
[send->] (assert (= v50 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bool )
[<-recv] success
[send->] (assert (= v51 (ite v49 true v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (ite v48 true v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 true v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (ite v53 true v56 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v52 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Int )
[<-recv] success
[send->] (assert (= v59 (- 4 v26 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Int )
[<-recv] success
[send->] (assert (= v60 (+ v35 v59 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Int )
[<-recv] success
[send->] (assert (= v61 (mod v60 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Int )
[<-recv] success
[send->] (assert (= v62 (ite v57 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Int )
[<-recv] success
[send->] (assert (= v63 (ite v52 2 v62 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (ite v58 v61 v63 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (= v64 1 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 true v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (ite v72 true v73 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (ite v71 true v74 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v70 false true ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v76 true v75 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v77 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 true v81 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 true v82 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v78 false true ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (ite v84 true v83 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Int )
[<-recv] success
[send->] (assert (= v87 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (+ v87 v20 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v89 v20 v91 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (ite v89 v90 v19 ) ) )
[<-recv] success
[send->] (declare-fun v94 () Int )
[<-recv] success
[send->] (assert (= v94 (ite v86 v88 v92 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Int )
[<-recv] success
[send->] (assert (= v95 (ite v86 0 v93 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v94 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v95 ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (= v64 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (assert (= v97 (= v64 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
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
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (declare-fun v17 () Bytes )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (<= 0 v23 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Int )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v27 ) )
[<-recv] success
[send->] (assert (<= 0 v27 ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v27 v19 ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (ite v30 true v31 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (assert (= v33 (ite v29 true v32 ) ) )
[<-recv] success
[send->] (assert v33 )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v36 ) )
[<-recv] success
[send->] (assert (<= 0 v36 ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v36 0 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (assert (= v38 (uint256->bytes v34 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bytes )
[<-recv] success
[send->] (assert (= v39 (uint256->bytes v35 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bytes )
[<-recv] success
[send->] (assert (= v40 (msg-cat v38 v39 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (assert (= v41 (digest v40 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (= v21 v41 ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 true v45 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 true v46 ) ) )
[<-recv] success
[send->] (assert v47 )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bool )
[<-recv] success
[send->] (assert (= v50 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bool )
[<-recv] success
[send->] (assert (= v51 (ite v49 true v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (ite v48 true v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 true v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (ite v53 true v56 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v52 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Int )
[<-recv] success
[send->] (assert (= v59 (- 4 v26 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Int )
[<-recv] success
[send->] (assert (= v60 (+ v35 v59 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Int )
[<-recv] success
[send->] (assert (= v61 (mod v60 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Int )
[<-recv] success
[send->] (assert (= v62 (ite v57 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Int )
[<-recv] success
[send->] (assert (= v63 (ite v52 2 v62 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (ite v58 v61 v63 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (= v64 1 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 true v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (ite v72 true v73 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (ite v71 true v74 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v70 false true ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v76 true v75 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v77 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 true v81 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 true v82 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v78 false true ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (ite v84 true v83 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Int )
[<-recv] success
[send->] (assert (= v87 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (+ v87 v20 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v89 v20 v91 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (ite v89 v90 v19 ) ) )
[<-recv] success
[send->] (declare-fun v94 () Int )
[<-recv] success
[send->] (assert (= v94 (ite v86 v88 v92 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Int )
[<-recv] success
[send->] (assert (= v95 (ite v86 0 v93 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v94 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v95 ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (= v64 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (assert (= v97 (= v64 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
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
[send->] (assert (= v4 (= v2 0 ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 1 ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (= v2 2 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v5 true v6 ) ) )
[<-recv] success
[send->] (declare-fun v8 () Bool )
[<-recv] success
[send->] (assert (= v8 (ite v4 true v7 ) ) )
[<-recv] success
[send->] (assert v8 )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v14 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (uint256->bytes v2 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Bytes )
[<-recv] success
[send->] (assert (= v17 (msg-cat v15 v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 (digest v17 ) ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v0 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v1 ) )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (assert (= v21 v18 ) )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (<= 0 v23 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Int )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v27 ) )
[<-recv] success
[send->] (assert (<= 0 v27 ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v27 v19 ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (ite v30 true v31 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (assert (= v33 (ite v29 true v32 ) ) )
[<-recv] success
[send->] (assert v33 )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (assert (= v34 v14 ) )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (assert (= v35 v2 ) )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v36 ) )
[<-recv] success
[send->] (assert (<= 0 v36 ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v36 0 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (assert (= v38 (uint256->bytes v34 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bytes )
[<-recv] success
[send->] (assert (= v39 (uint256->bytes v35 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bytes )
[<-recv] success
[send->] (assert (= v40 (msg-cat v38 v39 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (assert (= v41 (digest v40 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (= v21 v41 ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 true v45 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 true v46 ) ) )
[<-recv] success
[send->] (assert v47 )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bool )
[<-recv] success
[send->] (assert (= v50 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bool )
[<-recv] success
[send->] (assert (= v51 (ite v49 true v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (ite v48 true v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 true v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (ite v53 true v56 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v52 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Int )
[<-recv] success
[send->] (assert (= v59 (- 4 v26 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Int )
[<-recv] success
[send->] (assert (= v60 (+ v35 v59 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Int )
[<-recv] success
[send->] (assert (= v61 (mod v60 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Int )
[<-recv] success
[send->] (assert (= v62 (ite v57 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Int )
[<-recv] success
[send->] (assert (= v63 (ite v52 2 v62 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (ite v58 v61 v63 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (= v64 1 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 true v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (ite v72 true v73 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (ite v71 true v74 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v70 false true ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v76 true v75 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v77 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 true v81 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 true v82 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v78 false true ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (ite v84 true v83 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Int )
[<-recv] success
[send->] (assert (= v87 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (+ v87 v20 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v89 v20 v91 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (ite v89 v90 v19 ) ) )
[<-recv] success
[send->] (declare-fun v94 () Int )
[<-recv] success
[send->] (assert (= v94 (ite v86 v88 v92 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Int )
[<-recv] success
[send->] (assert (= v95 (ite v86 0 v93 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v94 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v95 ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (= v64 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (assert (= v97 (= v64 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
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
[send->] (assert (= v9 (= v3 0 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (= v3 1 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (= v3 2 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 true v11 ) ) )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (assert (= v13 (ite v9 true v12 ) ) )
[<-recv] success
[send->] (assert v13 )
[<-recv] success
[send->] (declare-fun v14 () Int )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (declare-fun v17 () Bytes )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (<= 0 v23 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Int )
[<-recv] success
[send->] (assert (= v26 v3 ) )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v27 ) )
[<-recv] success
[send->] (assert (<= 0 v27 ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v27 v19 ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (assert (= v32 (ite v30 true v31 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (assert (= v33 (ite v29 true v32 ) ) )
[<-recv] success
[send->] (assert v33 )
[<-recv] success
[send->] (declare-fun v34 () Int )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v36 ) )
[<-recv] success
[send->] (assert (<= 0 v36 ) )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (assert (= v37 (= v36 0 ) ) )
[<-recv] success
[send->] (assert v37 )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (assert (= v38 (uint256->bytes v34 ) ) )
[<-recv] success
[send->] (declare-fun v39 () Bytes )
[<-recv] success
[send->] (assert (= v39 (uint256->bytes v35 ) ) )
[<-recv] success
[send->] (declare-fun v40 () Bytes )
[<-recv] success
[send->] (assert (= v40 (msg-cat v38 v39 ) ) )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (assert (= v41 (digest v40 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (= v21 v41 ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (assert (= v44 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (assert (= v45 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v46 () Bool )
[<-recv] success
[send->] (assert (= v46 (ite v44 true v45 ) ) )
[<-recv] success
[send->] (declare-fun v47 () Bool )
[<-recv] success
[send->] (assert (= v47 (ite v43 true v46 ) ) )
[<-recv] success
[send->] (assert v47 )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v49 () Bool )
[<-recv] success
[send->] (assert (= v49 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bool )
[<-recv] success
[send->] (assert (= v50 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bool )
[<-recv] success
[send->] (assert (= v51 (ite v49 true v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Bool )
[<-recv] success
[send->] (assert (= v52 (ite v48 true v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 true v55 ) ) )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (ite v53 true v56 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (ite v52 v57 false ) ) )
[<-recv] success
[send->] (declare-fun v59 () Int )
[<-recv] success
[send->] (assert (= v59 (- 4 v26 ) ) )
[<-recv] success
[send->] (declare-fun v60 () Int )
[<-recv] success
[send->] (assert (= v60 (+ v35 v59 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Int )
[<-recv] success
[send->] (assert (= v61 (mod v60 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Int )
[<-recv] success
[send->] (assert (= v62 (ite v57 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v63 () Int )
[<-recv] success
[send->] (assert (= v63 (ite v52 2 v62 ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (ite v58 v61 v63 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Bool )
[<-recv] success
[send->] (assert (= v65 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Bool )
[<-recv] success
[send->] (assert (= v66 (= v64 1 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Bool )
[<-recv] success
[send->] (assert (= v67 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Bool )
[<-recv] success
[send->] (assert (= v68 (ite v66 true v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Bool )
[<-recv] success
[send->] (assert (= v69 (ite v65 true v68 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v69 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v69 )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (= v35 0 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (= v35 1 ) ) )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v35 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (ite v72 true v73 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (ite v71 true v74 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v70 false true ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v76 true v75 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v77 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v77 )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v26 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (= v26 1 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (= v26 2 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 true v81 ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 true v82 ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v78 false true ) ) )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (ite v84 true v83 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v85 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v85 )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (assert (= v86 (= v64 2 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Int )
[<-recv] success
[send->] (assert (= v87 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (+ v87 v20 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (assert (= v89 (= v64 0 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (* 2 v19 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (+ v19 v20 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v89 v20 v91 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (ite v89 v90 v19 ) ) )
[<-recv] success
[send->] (declare-fun v94 () Int )
[<-recv] success
[send->] (assert (= v94 (ite v86 v88 v92 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Int )
[<-recv] success
[send->] (assert (= v95 (ite v86 0 v93 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v94 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v95 ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (= v64 2 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (assert (= v97 (= v64 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v97 )
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
