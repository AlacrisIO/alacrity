[send->] (set-option :print-success true )
[<-recv] success
[send->] (set-option :produce-models true )
[<-recv] success
[send->] (set-option :produce-unsat-cores true )
[<-recv] success
[send->] (declare-datatypes () ((Bytes (bytes-literal (digest Int ) ) (uint256->bytes (bytes-integer Int ) ) (msg-cat (msg-left Bytes ) (msg-right Bytes ) ) ) ) )
[<-recv] success
[send->] (declare-fun digest (Bytes ) Int )
[<-recv] success
[send->] (define-fun-rec bytes-length ((bs Bytes ) ) Int (match bs (((uint256->bytes _ ) 32 ) ((bytes-literal _ ) 32 ) ((msg-cat l r ) (+ 32 (bytes-length l ) (bytes-length r ) ) ) ) ) )
[<-recv] success
[send->] (declare-fun v0 () Int )
[<-recv] success
[send->] (declare-fun v1 () Int )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (declare-fun v2 () Bytes )
[<-recv] success
[send->] (declare-fun v3 () Bool )
[<-recv] success
[send->] (assert (= v3 (= v2 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (= v2 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 true v5 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v3 true v6 ) ) )
[<-recv] success
[send->] (assert v7 )
[<-recv] success
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (assert (= v8 (ite v4 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Int )
[<-recv] success
[send->] (assert (= v9 (ite v3 0 v8 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (<= 0 v9 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (< v9 3 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 v11 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v12 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v12 )
[<-recv] success
[send->] (declare-fun v13 () Int )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v9 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (msg-cat v14 v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (digest v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v0 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v1 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v17 ) )
[<-recv] success
[send->] (declare-fun v21 () Bytes )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v18 v19 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (= v23 v22 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v18 v19 ) ) )
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
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (= v26 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v26 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (ite v28 true v29 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (ite v27 true v30 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Int )
[<-recv] success
[send->] (assert (= v32 (ite v28 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (assert (= v33 (ite v27 0 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (<= 0 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (< v33 3 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (ite v34 v35 false ) ) )
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
[send->] (declare-fun v37 () Int )
[<-recv] success
[send->] (assert (= v37 v33 ) )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (declare-fun v39 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v39 ) )
[<-recv] success
[send->] (assert (= v39 v18 ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (= v39 v18 ) ) )
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
[send->] (assert (= v41 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v43 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v43 )
[<-recv] success
[send->] (declare-fun v44 () Int )
[<-recv] success
[send->] (assert (= v44 v13 ) )
[<-recv] success
[send->] (declare-fun v45 () Int )
[<-recv] success
[send->] (assert (= v45 v9 ) )
[<-recv] success
[send->] (declare-fun v46 () Bytes )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v47 ) )
[<-recv] success
[send->] (assert (= v47 0 ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v48 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (declare-fun v49 () Bytes )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v44 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bytes )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v45 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bytes )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v20 v52 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v53 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v45 3 ) ) )
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
[send->] (assert (= v57 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (- 4 v37 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (assert (= v65 (+ v45 v64 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Int )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Int )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (ite v77 true v76 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Int )
[<-recv] success
[send->] (assert (= v86 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Int )
[<-recv] success
[send->] (assert (= v89 (ite v87 0 v18 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (ite v87 v88 v18 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (ite v85 v86 v89 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v85 0 v90 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (+ v19 v91 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v93 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v92 ) )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (assert (= v94 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (assert (= v95 (= v45 0 ) ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (ite v95 v94 false ) ) )
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
[send->] (assert (= v97 (= v45 1 ) ) )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (assert (= v98 (ite v97 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (assert (= v99 (= v45 2 ) ) )
[<-recv] success
[send->] (declare-fun v100 () Bool )
[<-recv] success
[send->] (assert (= v100 (ite v99 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v102 () Bool )
[<-recv] success
[send->] (assert (= v102 (= v37 0 ) ) )
[<-recv] success
[send->] (declare-fun v103 () Bool )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v104 () Bool )
[<-recv] success
[send->] (assert (= v104 (= v37 1 ) ) )
[<-recv] success
[send->] (declare-fun v105 () Bool )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v106 () Bool )
[<-recv] success
[send->] (assert (= v106 (= v37 2 ) ) )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v108 () Bytes )
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
[send->] (declare-fun v2 () Bytes )
[<-recv] success
[send->] (declare-fun v3 () Bool )
[<-recv] success
[send->] (assert (= v3 (= v2 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (= v2 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 true v5 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v3 true v6 ) ) )
[<-recv] success
[send->] (assert v7 )
[<-recv] success
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (assert (= v8 (ite v4 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Int )
[<-recv] success
[send->] (assert (= v9 (ite v3 0 v8 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (<= 0 v9 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (< v9 3 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 v11 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v12 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v12 )
[<-recv] success
[send->] (declare-fun v13 () Int )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v9 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (msg-cat v14 v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (digest v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v0 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v1 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v17 ) )
[<-recv] success
[send->] (declare-fun v21 () Bytes )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v18 v19 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (= v23 v22 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v18 v19 ) ) )
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
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (= v26 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v26 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (ite v28 true v29 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (ite v27 true v30 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Int )
[<-recv] success
[send->] (assert (= v32 (ite v28 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (assert (= v33 (ite v27 0 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (<= 0 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (< v33 3 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (ite v34 v35 false ) ) )
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
[send->] (declare-fun v37 () Int )
[<-recv] success
[send->] (assert (= v37 v33 ) )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (declare-fun v39 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v39 ) )
[<-recv] success
[send->] (assert (= v39 v18 ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (= v39 v18 ) ) )
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
[send->] (assert (= v41 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v43 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v43 )
[<-recv] success
[send->] (declare-fun v44 () Int )
[<-recv] success
[send->] (assert (= v44 v13 ) )
[<-recv] success
[send->] (declare-fun v45 () Int )
[<-recv] success
[send->] (assert (= v45 v9 ) )
[<-recv] success
[send->] (declare-fun v46 () Bytes )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v47 ) )
[<-recv] success
[send->] (assert (= v47 0 ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v48 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (declare-fun v49 () Bytes )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v44 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bytes )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v45 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bytes )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v20 v52 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v53 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v45 3 ) ) )
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
[send->] (assert (= v57 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (- 4 v37 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (assert (= v65 (+ v45 v64 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Int )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Int )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (ite v77 true v76 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Int )
[<-recv] success
[send->] (assert (= v86 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Int )
[<-recv] success
[send->] (assert (= v89 (ite v87 0 v18 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (ite v87 v88 v18 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (ite v85 v86 v89 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v85 0 v90 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (+ v19 v91 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v93 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v92 ) )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (assert (= v94 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (assert (= v95 (= v45 0 ) ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (ite v95 v94 false ) ) )
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
[send->] (assert (= v97 (= v45 1 ) ) )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (assert (= v98 (ite v97 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (assert (= v99 (= v45 2 ) ) )
[<-recv] success
[send->] (declare-fun v100 () Bool )
[<-recv] success
[send->] (assert (= v100 (ite v99 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v102 () Bool )
[<-recv] success
[send->] (assert (= v102 (= v37 0 ) ) )
[<-recv] success
[send->] (declare-fun v103 () Bool )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v104 () Bool )
[<-recv] success
[send->] (assert (= v104 (= v37 1 ) ) )
[<-recv] success
[send->] (declare-fun v105 () Bool )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v106 () Bool )
[<-recv] success
[send->] (assert (= v106 (= v37 2 ) ) )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v108 () Bytes )
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
[send->] (declare-fun v2 () Bytes )
[<-recv] success
[send->] (declare-fun v3 () Bool )
[<-recv] success
[send->] (assert (= v3 (= v2 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (= v2 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 true v5 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v3 true v6 ) ) )
[<-recv] success
[send->] (assert v7 )
[<-recv] success
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (assert (= v8 (ite v4 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Int )
[<-recv] success
[send->] (assert (= v9 (ite v3 0 v8 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (<= 0 v9 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (< v9 3 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 v11 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v12 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v12 )
[<-recv] success
[send->] (declare-fun v13 () Int )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v9 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (msg-cat v14 v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (digest v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v0 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v1 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v17 ) )
[<-recv] success
[send->] (declare-fun v21 () Bytes )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v18 v19 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (= v23 v22 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v18 v19 ) ) )
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
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (= v26 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v26 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (ite v28 true v29 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (ite v27 true v30 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Int )
[<-recv] success
[send->] (assert (= v32 (ite v28 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (assert (= v33 (ite v27 0 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (<= 0 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (< v33 3 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (ite v34 v35 false ) ) )
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
[send->] (declare-fun v37 () Int )
[<-recv] success
[send->] (assert (= v37 v33 ) )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (declare-fun v39 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v39 ) )
[<-recv] success
[send->] (assert (= v39 v18 ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (= v39 v18 ) ) )
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
[send->] (assert (= v41 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v43 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v43 )
[<-recv] success
[send->] (declare-fun v44 () Int )
[<-recv] success
[send->] (assert (= v44 v13 ) )
[<-recv] success
[send->] (declare-fun v45 () Int )
[<-recv] success
[send->] (assert (= v45 v9 ) )
[<-recv] success
[send->] (declare-fun v46 () Bytes )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v47 ) )
[<-recv] success
[send->] (assert (= v47 0 ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v48 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (declare-fun v49 () Bytes )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v44 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bytes )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v45 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bytes )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v20 v52 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v53 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v45 3 ) ) )
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
[send->] (assert (= v57 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (- 4 v37 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (assert (= v65 (+ v45 v64 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Int )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Int )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (ite v77 true v76 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Int )
[<-recv] success
[send->] (assert (= v86 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Int )
[<-recv] success
[send->] (assert (= v89 (ite v87 0 v18 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (ite v87 v88 v18 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (ite v85 v86 v89 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v85 0 v90 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (+ v19 v91 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v93 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v92 ) )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (assert (= v94 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (assert (= v95 (= v45 0 ) ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (ite v95 v94 false ) ) )
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
[send->] (assert (= v97 (= v45 1 ) ) )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (assert (= v98 (ite v97 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (assert (= v99 (= v45 2 ) ) )
[<-recv] success
[send->] (declare-fun v100 () Bool )
[<-recv] success
[send->] (assert (= v100 (ite v99 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v102 () Bool )
[<-recv] success
[send->] (assert (= v102 (= v37 0 ) ) )
[<-recv] success
[send->] (declare-fun v103 () Bool )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v104 () Bool )
[<-recv] success
[send->] (assert (= v104 (= v37 1 ) ) )
[<-recv] success
[send->] (declare-fun v105 () Bool )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v106 () Bool )
[<-recv] success
[send->] (assert (= v106 (= v37 2 ) ) )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v108 () Bytes )
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
[send->] (declare-fun v2 () Bytes )
[<-recv] success
[send->] (declare-fun v3 () Bool )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (declare-fun v9 () Int )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (declare-fun v13 () Int )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (declare-fun v21 () Bytes )
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
[send->] (assert (= v24 (+ v18 v19 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (declare-fun v32 () Int )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (declare-fun v37 () Int )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (declare-fun v39 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v39 ) )
[<-recv] success
[send->] (assert (<= 0 v39 ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (= v39 v18 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (assert v43 )
[<-recv] success
[send->] (declare-fun v44 () Int )
[<-recv] success
[send->] (declare-fun v45 () Int )
[<-recv] success
[send->] (declare-fun v46 () Bytes )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v47 ) )
[<-recv] success
[send->] (assert (<= 0 v47 ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (declare-fun v49 () Bytes )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v44 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bytes )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v45 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bytes )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v20 v52 ) ) )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (- 4 v37 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (assert (= v65 (+ v45 v64 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Int )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Int )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (ite v77 true v76 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Int )
[<-recv] success
[send->] (assert (= v86 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Int )
[<-recv] success
[send->] (assert (= v89 (ite v87 0 v18 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (ite v87 v88 v18 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (ite v85 v86 v89 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v85 0 v90 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (+ v19 v91 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v93 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v92 ) )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (assert (= v94 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (assert (= v95 (= v45 0 ) ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (ite v95 v94 false ) ) )
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
[send->] (assert (= v97 (= v45 1 ) ) )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (assert (= v98 (ite v97 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (assert (= v99 (= v45 2 ) ) )
[<-recv] success
[send->] (declare-fun v100 () Bool )
[<-recv] success
[send->] (assert (= v100 (ite v99 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v102 () Bool )
[<-recv] success
[send->] (assert (= v102 (= v37 0 ) ) )
[<-recv] success
[send->] (declare-fun v103 () Bool )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v104 () Bool )
[<-recv] success
[send->] (assert (= v104 (= v37 1 ) ) )
[<-recv] success
[send->] (declare-fun v105 () Bool )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v106 () Bool )
[<-recv] success
[send->] (assert (= v106 (= v37 2 ) ) )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v108 () Bytes )
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
[send->] (declare-fun v2 () Bytes )
[<-recv] success
[send->] (declare-fun v3 () Bool )
[<-recv] success
[send->] (assert (= v3 (= v2 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (assert (= v4 (= v2 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (assert (= v5 (= v2 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (assert (= v6 (ite v4 true v5 ) ) )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (assert (= v7 (ite v3 true v6 ) ) )
[<-recv] success
[send->] (assert v7 )
[<-recv] success
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (assert (= v8 (ite v4 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v9 () Int )
[<-recv] success
[send->] (assert (= v9 (ite v3 0 v8 ) ) )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (assert (= v10 (<= 0 v9 ) ) )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (assert (= v11 (< v9 3 ) ) )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (assert (= v12 (ite v10 v11 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v12 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v12 )
[<-recv] success
[send->] (declare-fun v13 () Int )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (assert (= v14 (uint256->bytes v13 ) ) )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (assert (= v15 (uint256->bytes v9 ) ) )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (assert (= v16 (msg-cat v14 v15 ) ) )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (assert (= v17 (digest v16 ) ) )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (assert (= v18 v0 ) )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (assert (= v19 v1 ) )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (assert (= v20 v17 ) )
[<-recv] success
[send->] (declare-fun v21 () Bytes )
[<-recv] success
[send->] (declare-fun v22 () Int )
[<-recv] success
[send->] (assert (= v22 (+ v18 v19 ) ) )
[<-recv] success
[send->] (declare-fun v23 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 v23 ) )
[<-recv] success
[send->] (assert (<= 0 v23 ) )
[<-recv] success
[send->] (declare-fun v24 () Int )
[<-recv] success
[send->] (assert (= v24 (+ v18 v19 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (declare-fun v32 () Int )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (declare-fun v37 () Int )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (declare-fun v39 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v39 ) )
[<-recv] success
[send->] (assert (<= 0 v39 ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (= v39 v18 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (assert v43 )
[<-recv] success
[send->] (declare-fun v44 () Int )
[<-recv] success
[send->] (assert (= v44 v13 ) )
[<-recv] success
[send->] (declare-fun v45 () Int )
[<-recv] success
[send->] (assert (= v45 v9 ) )
[<-recv] success
[send->] (declare-fun v46 () Bytes )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v47 ) )
[<-recv] success
[send->] (assert (<= 0 v47 ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (declare-fun v49 () Bytes )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v44 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bytes )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v45 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bytes )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v20 v52 ) ) )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (- 4 v37 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (assert (= v65 (+ v45 v64 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Int )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Int )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (ite v77 true v76 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Int )
[<-recv] success
[send->] (assert (= v86 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Int )
[<-recv] success
[send->] (assert (= v89 (ite v87 0 v18 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (ite v87 v88 v18 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (ite v85 v86 v89 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v85 0 v90 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (+ v19 v91 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v93 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v92 ) )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (assert (= v94 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (assert (= v95 (= v45 0 ) ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (ite v95 v94 false ) ) )
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
[send->] (assert (= v97 (= v45 1 ) ) )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (assert (= v98 (ite v97 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (assert (= v99 (= v45 2 ) ) )
[<-recv] success
[send->] (declare-fun v100 () Bool )
[<-recv] success
[send->] (assert (= v100 (ite v99 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v102 () Bool )
[<-recv] success
[send->] (assert (= v102 (= v37 0 ) ) )
[<-recv] success
[send->] (declare-fun v103 () Bool )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v104 () Bool )
[<-recv] success
[send->] (assert (= v104 (= v37 1 ) ) )
[<-recv] success
[send->] (declare-fun v105 () Bool )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v106 () Bool )
[<-recv] success
[send->] (assert (= v106 (= v37 2 ) ) )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v108 () Bytes )
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
[send->] (declare-fun v2 () Bytes )
[<-recv] success
[send->] (declare-fun v3 () Bool )
[<-recv] success
[send->] (declare-fun v4 () Bool )
[<-recv] success
[send->] (declare-fun v5 () Bool )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (declare-fun v7 () Bool )
[<-recv] success
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (declare-fun v9 () Int )
[<-recv] success
[send->] (declare-fun v10 () Bool )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (declare-fun v13 () Int )
[<-recv] success
[send->] (declare-fun v14 () Bytes )
[<-recv] success
[send->] (declare-fun v15 () Bytes )
[<-recv] success
[send->] (declare-fun v16 () Bytes )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (declare-fun v18 () Int )
[<-recv] success
[send->] (declare-fun v19 () Int )
[<-recv] success
[send->] (declare-fun v20 () Int )
[<-recv] success
[send->] (declare-fun v21 () Bytes )
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
[send->] (assert (= v24 (+ v18 v19 ) ) )
[<-recv] success
[send->] (declare-fun v25 () Bool )
[<-recv] success
[send->] (assert (= v25 (= v23 v24 ) ) )
[<-recv] success
[send->] (assert v25 )
[<-recv] success
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Bool )
[<-recv] success
[send->] (assert (= v27 (= v26 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (assert (= v28 (= v26 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (declare-fun v29 () Bool )
[<-recv] success
[send->] (assert (= v29 (= v26 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (assert (= v30 (ite v28 true v29 ) ) )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (assert (= v31 (ite v27 true v30 ) ) )
[<-recv] success
[send->] (assert v31 )
[<-recv] success
[send->] (declare-fun v32 () Int )
[<-recv] success
[send->] (assert (= v32 (ite v28 1 2 ) ) )
[<-recv] success
[send->] (declare-fun v33 () Int )
[<-recv] success
[send->] (assert (= v33 (ite v27 0 v32 ) ) )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (assert (= v34 (<= 0 v33 ) ) )
[<-recv] success
[send->] (declare-fun v35 () Bool )
[<-recv] success
[send->] (assert (= v35 (< v33 3 ) ) )
[<-recv] success
[send->] (declare-fun v36 () Bool )
[<-recv] success
[send->] (assert (= v36 (ite v34 v35 false ) ) )
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
[send->] (declare-fun v37 () Int )
[<-recv] success
[send->] (assert (= v37 v33 ) )
[<-recv] success
[send->] (declare-fun v38 () Bytes )
[<-recv] success
[send->] (declare-fun v39 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 v39 ) )
[<-recv] success
[send->] (assert (<= 0 v39 ) )
[<-recv] success
[send->] (declare-fun v40 () Bool )
[<-recv] success
[send->] (assert (= v40 (= v39 v18 ) ) )
[<-recv] success
[send->] (assert v40 )
[<-recv] success
[send->] (declare-fun v41 () Bool )
[<-recv] success
[send->] (assert (= v41 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (assert (= v42 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (assert (= v43 (ite v41 v42 false ) ) )
[<-recv] success
[send->] (assert v43 )
[<-recv] success
[send->] (declare-fun v44 () Int )
[<-recv] success
[send->] (declare-fun v45 () Int )
[<-recv] success
[send->] (declare-fun v46 () Bytes )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 v47 ) )
[<-recv] success
[send->] (assert (<= 0 v47 ) )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (declare-fun v49 () Bytes )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v44 ) ) )
[<-recv] success
[send->] (declare-fun v50 () Bytes )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v45 ) ) )
[<-recv] success
[send->] (declare-fun v51 () Bytes )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (assert (= v53 (= v20 v52 ) ) )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (assert (= v54 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (assert (= v55 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (assert (= v57 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (assert (= v58 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (assert (= v60 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (assert (= v61 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (assert (= v64 (- 4 v37 ) ) )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (assert (= v65 (+ v45 v64 ) ) )
[<-recv] success
[send->] (declare-fun v66 () Int )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (declare-fun v67 () Int )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (assert (= v72 (ite v70 v71 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v72 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v72 )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (assert (= v74 (<= 0 v45 ) ) )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (assert (= v75 (< v45 3 ) ) )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (assert (= v78 (ite v77 true v76 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v78 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v78 )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (assert (= v80 (<= 0 v37 ) ) )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (assert (= v81 (< v37 3 ) ) )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (assert (= v84 (ite v83 true v82 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v84 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v84 )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v86 () Int )
[<-recv] success
[send->] (assert (= v86 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (assert (= v87 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v88 () Int )
[<-recv] success
[send->] (assert (= v88 (* 2 v18 ) ) )
[<-recv] success
[send->] (declare-fun v89 () Int )
[<-recv] success
[send->] (assert (= v89 (ite v87 0 v18 ) ) )
[<-recv] success
[send->] (declare-fun v90 () Int )
[<-recv] success
[send->] (assert (= v90 (ite v87 v88 v18 ) ) )
[<-recv] success
[send->] (declare-fun v91 () Int )
[<-recv] success
[send->] (assert (= v91 (ite v85 v86 v89 ) ) )
[<-recv] success
[send->] (declare-fun v92 () Int )
[<-recv] success
[send->] (assert (= v92 (ite v85 0 v90 ) ) )
[<-recv] success
[send->] (declare-fun v93 () Int )
[<-recv] success
[send->] (assert (= v93 (+ v19 v91 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (- ctc_balance3 v93 ) )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (- ctc_balance4 v92 ) )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (assert (= v94 (= v69 2 ) ) )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (assert (= v95 (= v45 0 ) ) )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (assert (= v96 (ite v95 v94 false ) ) )
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
[send->] (assert (= v97 (= v45 1 ) ) )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (assert (= v98 (ite v97 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (assert (= v99 (= v45 2 ) ) )
[<-recv] success
[send->] (declare-fun v100 () Bool )
[<-recv] success
[send->] (assert (= v100 (ite v99 v94 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v100 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (declare-fun v102 () Bool )
[<-recv] success
[send->] (assert (= v102 (= v37 0 ) ) )
[<-recv] success
[send->] (declare-fun v103 () Bool )
[<-recv] success
[send->] (assert (= v103 (ite v102 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v103 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v104 () Bool )
[<-recv] success
[send->] (assert (= v104 (= v37 1 ) ) )
[<-recv] success
[send->] (declare-fun v105 () Bool )
[<-recv] success
[send->] (assert (= v105 (ite v104 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v105 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v106 () Bool )
[<-recv] success
[send->] (assert (= v106 (= v37 2 ) ) )
[<-recv] success
[send->] (declare-fun v107 () Bool )
[<-recv] success
[send->] (assert (= v107 (ite v106 v101 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v107 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (declare-fun v108 () Bytes )
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
