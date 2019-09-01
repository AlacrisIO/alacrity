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
[send->] (declare-fun v2 () Bytes )
[<-recv] success
[send->] (declare-fun v3 () Int )
[<-recv] success
[send->] (declare-fun v4 () Int )
[<-recv] success
[send->] (declare-fun v5 () Int )
[<-recv] success
[send->] (declare-fun v6 () Bool )
[<-recv] success
[send->] (declare-fun v7 () Bytes )
[<-recv] success
[send->] (declare-fun v8 () Int )
[<-recv] success
[send->] (declare-fun v9 () Bool )
[<-recv] success
[send->] (declare-fun v10 () Bytes )
[<-recv] success
[send->] (declare-fun v11 () Bool )
[<-recv] success
[send->] (declare-fun v12 () Bool )
[<-recv] success
[send->] (declare-fun v13 () Bool )
[<-recv] success
[send->] (declare-fun v14 () Bool )
[<-recv] success
[send->] (declare-fun v15 () Bool )
[<-recv] success
[send->] (declare-fun v16 () Int )
[<-recv] success
[send->] (declare-fun v17 () Int )
[<-recv] success
[send->] (declare-fun v18 () Bool )
[<-recv] success
[send->] (declare-fun v19 () Bool )
[<-recv] success
[send->] (declare-fun v20 () Bool )
[<-recv] success
[send->] (declare-fun v21 () Int )
[<-recv] success
[send->] (declare-fun v22 () Bytes )
[<-recv] success
[send->] (declare-fun v23 () Bytes )
[<-recv] success
[send->] (declare-fun v24 () Bytes )
[<-recv] success
[send->] (declare-fun v25 () Int )
[<-recv] success
[send->] (declare-fun v26 () Bytes )
[<-recv] success
[send->] (declare-fun v27 () Int )
[<-recv] success
[send->] (declare-fun v28 () Bool )
[<-recv] success
[send->] (declare-fun v29 () Bytes )
[<-recv] success
[send->] (declare-fun v30 () Bool )
[<-recv] success
[send->] (declare-fun v31 () Bool )
[<-recv] success
[send->] (declare-fun v32 () Bool )
[<-recv] success
[send->] (declare-fun v33 () Bool )
[<-recv] success
[send->] (declare-fun v34 () Bool )
[<-recv] success
[send->] (declare-fun v35 () Int )
[<-recv] success
[send->] (declare-fun v36 () Int )
[<-recv] success
[send->] (declare-fun v37 () Bool )
[<-recv] success
[send->] (declare-fun v38 () Bool )
[<-recv] success
[send->] (declare-fun v39 () Bool )
[<-recv] success
[send->] (declare-fun v40 () Bytes )
[<-recv] success
[send->] (declare-fun v41 () Int )
[<-recv] success
[send->] (declare-fun v42 () Bool )
[<-recv] success
[send->] (declare-fun v43 () Bool )
[<-recv] success
[send->] (declare-fun v44 () Bool )
[<-recv] success
[send->] (declare-fun v45 () Bool )
[<-recv] success
[send->] (declare-fun v46 () Bytes )
[<-recv] success
[send->] (declare-fun v47 () Int )
[<-recv] success
[send->] (declare-fun v48 () Bool )
[<-recv] success
[send->] (declare-fun v49 () Bytes )
[<-recv] success
[send->] (declare-fun v50 () Bytes )
[<-recv] success
[send->] (declare-fun v51 () Bytes )
[<-recv] success
[send->] (declare-fun v52 () Int )
[<-recv] success
[send->] (declare-fun v53 () Bool )
[<-recv] success
[send->] (declare-fun v54 () Bool )
[<-recv] success
[send->] (declare-fun v55 () Bool )
[<-recv] success
[send->] (declare-fun v56 () Bool )
[<-recv] success
[send->] (declare-fun v57 () Bool )
[<-recv] success
[send->] (declare-fun v58 () Bool )
[<-recv] success
[send->] (declare-fun v59 () Bool )
[<-recv] success
[send->] (declare-fun v60 () Bool )
[<-recv] success
[send->] (declare-fun v61 () Bool )
[<-recv] success
[send->] (declare-fun v62 () Bool )
[<-recv] success
[send->] (declare-fun v63 () Bool )
[<-recv] success
[send->] (declare-fun v64 () Int )
[<-recv] success
[send->] (declare-fun v65 () Int )
[<-recv] success
[send->] (declare-fun v66 () Int )
[<-recv] success
[send->] (declare-fun v67 () Int )
[<-recv] success
[send->] (declare-fun v68 () Int )
[<-recv] success
[send->] (declare-fun v69 () Int )
[<-recv] success
[send->] (declare-fun v70 () Bool )
[<-recv] success
[send->] (declare-fun v71 () Bool )
[<-recv] success
[send->] (declare-fun v72 () Bool )
[<-recv] success
[send->] (declare-fun v73 () Bool )
[<-recv] success
[send->] (declare-fun v74 () Bool )
[<-recv] success
[send->] (declare-fun v75 () Bool )
[<-recv] success
[send->] (declare-fun v76 () Bool )
[<-recv] success
[send->] (declare-fun v77 () Bool )
[<-recv] success
[send->] (declare-fun v78 () Bool )
[<-recv] success
[send->] (declare-fun v79 () Bool )
[<-recv] success
[send->] (declare-fun v80 () Bool )
[<-recv] success
[send->] (declare-fun v81 () Bool )
[<-recv] success
[send->] (declare-fun v82 () Bool )
[<-recv] success
[send->] (declare-fun v83 () Bool )
[<-recv] success
[send->] (declare-fun v84 () Bool )
[<-recv] success
[send->] (declare-fun v85 () Bool )
[<-recv] success
[send->] (declare-fun v86 () Bool )
[<-recv] success
[send->] (declare-fun v87 () Bool )
[<-recv] success
[send->] (declare-fun v88 () Bool )
[<-recv] success
[send->] (declare-fun v89 () Bool )
[<-recv] success
[send->] (declare-fun v90 () Bool )
[<-recv] success
[send->] (declare-fun v91 () Bool )
[<-recv] success
[send->] (declare-fun v92 () Bool )
[<-recv] success
[send->] (declare-fun v93 () Bool )
[<-recv] success
[send->] (declare-fun v94 () Bool )
[<-recv] success
[send->] (declare-fun v95 () Bool )
[<-recv] success
[send->] (declare-fun v96 () Bool )
[<-recv] success
[send->] (declare-fun v97 () Bool )
[<-recv] success
[send->] (declare-fun v98 () Bool )
[<-recv] success
[send->] (declare-fun v99 () Bool )
[<-recv] success
[send->] (declare-fun v100 () Int )
[<-recv] success
[send->] (declare-fun v101 () Bool )
[<-recv] success
[send->] (declare-fun v102 () Int )
[<-recv] success
[send->] (declare-fun v103 () Int )
[<-recv] success
[send->] (declare-fun v104 () Int )
[<-recv] success
[send->] (declare-fun v105 () Int )
[<-recv] success
[send->] (declare-fun v106 () Int )
[<-recv] success
[send->] (declare-fun v107 () Int )
[<-recv] success
[send->] (declare-fun v108 () Bytes )
[<-recv] success
[send->] (declare-fun v109 () Bool )
[<-recv] success
[send->] (declare-fun v110 () Bool )
[<-recv] success
[send->] (declare-fun v111 () Bool )
[<-recv] success
[send->] (declare-fun v112 () Bool )
[<-recv] success
[send->] (declare-fun v113 () Bool )
[<-recv] success
[send->] (declare-fun v114 () Bytes )
[<-recv] success
[send->] (declare-fun v115 () Bytes )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v3 (+ v0 v1 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (= txn_value1 v3 ) )
[<-recv] success
[send->] (assert (= v4 txn_value1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v0 v1 ) ) )
[<-recv] success
[send->] (assert (= v6 (= v4 v5 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v6 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (= txn_value2 v0 ) )
[<-recv] success
[send->] (assert (= v8 txn_value2 ) )
[<-recv] success
[send->] (assert (= v9 (= v8 v0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v9 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (assert (= v11 (= v10 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v12 (= v10 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v13 (= v10 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (ite v12 true v13 ) ) )
[<-recv] success
[send->] (assert (= v15 (ite v11 true v14 ) ) )
[<-recv] success
[send->] (assert v15 )
[<-recv] success
[send->] (assert (= v16 (ite v12 1 2 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v11 0 v16 ) ) )
[<-recv] success
[send->] (assert (= v18 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v19 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v18 v19 false ) ) )
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
[send->] (assert (= v22 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v24 (msg-cat v22 v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (digest v24 ) ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (= txn_value3 0 ) )
[<-recv] success
[send->] (assert (= v27 txn_value3 ) )
[<-recv] success
[send->] (assert (= v28 (= v27 0 ) ) )
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
[send->] (assert (= v30 (= v29 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v31 (= v29 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v32 (= v29 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v33 (ite v31 true v32 ) ) )
[<-recv] success
[send->] (assert (= v34 (ite v30 true v33 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v35 (ite v31 1 2 ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v30 0 v35 ) ) )
[<-recv] success
[send->] (assert (= v37 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v38 (< v36 3 ) ) )
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
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (= txn_value4 0 ) )
[<-recv] success
[send->] (assert (= v41 txn_value4 ) )
[<-recv] success
[send->] (assert (= v42 (= v41 0 ) ) )
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
[send->] (assert (= v43 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v47 txn_value5 ) )
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
[send->] (assert (= v49 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (assert (= v53 (= v25 v52 ) ) )
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
[send->] (assert (= v54 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v55 (< v17 3 ) ) )
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
[send->] (assert (= v57 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v58 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (assert (= v60 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (assert (= v64 (- 4 v36 ) ) )
[<-recv] success
[send->] (assert (= v65 (+ v17 v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
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
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v74 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v75 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
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
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
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
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v86 (= v17 0 ) ) )
[<-recv] success
[send->] (assert (= v87 (ite v86 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v87 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v88 (= v17 1 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v88 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v90 (= v17 2 ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v92 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (= v36 0 ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v95 (= v36 1 ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v95 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v97 (= v36 2 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v97 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v99 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v100 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v102 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v101 0 v0 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v101 v102 v0 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v99 v100 v103 ) ) )
[<-recv] success
[send->] (assert (= v106 (ite v99 0 v104 ) ) )
[<-recv] success
[send->] (assert (= v107 (+ v1 v105 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v107 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v106 ) )
[<-recv] success
[send->] (assert (= v109 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v110 (< v69 3 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v109 v110 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v111 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (assert (= v112 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v113 (= v69 1 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v115 (ite v112 (bytes-literal 1946528133 ) v114 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v3 (+ v0 v1 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (= txn_value1 v3 ) )
[<-recv] success
[send->] (assert (= v4 txn_value1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v0 v1 ) ) )
[<-recv] success
[send->] (assert (= v6 (= v4 v5 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v6 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (= txn_value2 v0 ) )
[<-recv] success
[send->] (assert (= v8 txn_value2 ) )
[<-recv] success
[send->] (assert (= v9 (= v8 v0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v9 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (assert (= v11 (= v10 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v12 (= v10 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v13 (= v10 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (ite v12 true v13 ) ) )
[<-recv] success
[send->] (assert (= v15 (ite v11 true v14 ) ) )
[<-recv] success
[send->] (assert v15 )
[<-recv] success
[send->] (assert (= v16 (ite v12 1 2 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v11 0 v16 ) ) )
[<-recv] success
[send->] (assert (= v18 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v19 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v18 v19 false ) ) )
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
[send->] (assert (= v22 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v24 (msg-cat v22 v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (digest v24 ) ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (= txn_value3 0 ) )
[<-recv] success
[send->] (assert (= v27 txn_value3 ) )
[<-recv] success
[send->] (assert (= v28 (= v27 0 ) ) )
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
[send->] (assert (= v30 (= v29 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v31 (= v29 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v32 (= v29 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v33 (ite v31 true v32 ) ) )
[<-recv] success
[send->] (assert (= v34 (ite v30 true v33 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v35 (ite v31 1 2 ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v30 0 v35 ) ) )
[<-recv] success
[send->] (assert (= v37 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v38 (< v36 3 ) ) )
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
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (= txn_value4 0 ) )
[<-recv] success
[send->] (assert (= v41 txn_value4 ) )
[<-recv] success
[send->] (assert (= v42 (= v41 0 ) ) )
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
[send->] (assert (= v43 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v47 txn_value5 ) )
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
[send->] (assert (= v49 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (assert (= v53 (= v25 v52 ) ) )
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
[send->] (assert (= v54 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v55 (< v17 3 ) ) )
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
[send->] (assert (= v57 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v58 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (assert (= v60 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (assert (= v64 (- 4 v36 ) ) )
[<-recv] success
[send->] (assert (= v65 (+ v17 v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
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
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v74 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v75 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
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
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
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
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v86 (= v17 0 ) ) )
[<-recv] success
[send->] (assert (= v87 (ite v86 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v87 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v88 (= v17 1 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v88 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v90 (= v17 2 ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v92 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (= v36 0 ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v95 (= v36 1 ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v95 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v97 (= v36 2 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v97 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v99 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v100 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v102 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v101 0 v0 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v101 v102 v0 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v99 v100 v103 ) ) )
[<-recv] success
[send->] (assert (= v106 (ite v99 0 v104 ) ) )
[<-recv] success
[send->] (assert (= v107 (+ v1 v105 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v107 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v106 ) )
[<-recv] success
[send->] (assert (= v109 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v110 (< v69 3 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v109 v110 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v111 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (assert (= v112 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v113 (= v69 1 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v115 (ite v112 (bytes-literal 1946528133 ) v114 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v3 (+ v0 v1 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (= txn_value1 v3 ) )
[<-recv] success
[send->] (assert (= v4 txn_value1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v0 v1 ) ) )
[<-recv] success
[send->] (assert (= v6 (= v4 v5 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v6 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (= txn_value2 v0 ) )
[<-recv] success
[send->] (assert (= v8 txn_value2 ) )
[<-recv] success
[send->] (assert (= v9 (= v8 v0 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v9 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (assert (= v11 (= v10 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v12 (= v10 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v13 (= v10 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (ite v12 true v13 ) ) )
[<-recv] success
[send->] (assert (= v15 (ite v11 true v14 ) ) )
[<-recv] success
[send->] (assert v15 )
[<-recv] success
[send->] (assert (= v16 (ite v12 1 2 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v11 0 v16 ) ) )
[<-recv] success
[send->] (assert (= v18 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v19 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v18 v19 false ) ) )
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
[send->] (assert (= v22 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v24 (msg-cat v22 v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (digest v24 ) ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (= txn_value3 0 ) )
[<-recv] success
[send->] (assert (= v27 txn_value3 ) )
[<-recv] success
[send->] (assert (= v28 (= v27 0 ) ) )
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
[send->] (assert (= v30 (= v29 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v31 (= v29 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v32 (= v29 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v33 (ite v31 true v32 ) ) )
[<-recv] success
[send->] (assert (= v34 (ite v30 true v33 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v35 (ite v31 1 2 ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v30 0 v35 ) ) )
[<-recv] success
[send->] (assert (= v37 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v38 (< v36 3 ) ) )
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
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (= txn_value4 0 ) )
[<-recv] success
[send->] (assert (= v41 txn_value4 ) )
[<-recv] success
[send->] (assert (= v42 (= v41 0 ) ) )
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
[send->] (assert (= v43 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v45 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (= txn_value5 0 ) )
[<-recv] success
[send->] (assert (= v47 txn_value5 ) )
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
[send->] (assert (= v49 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (assert (= v53 (= v25 v52 ) ) )
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
[send->] (assert (= v54 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v55 (< v17 3 ) ) )
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
[send->] (assert (= v57 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v58 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (assert (= v60 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (assert (= v64 (- 4 v36 ) ) )
[<-recv] success
[send->] (assert (= v65 (+ v17 v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
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
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v74 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v75 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
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
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
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
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v86 (= v17 0 ) ) )
[<-recv] success
[send->] (assert (= v87 (ite v86 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v87 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v88 (= v17 1 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v88 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v90 (= v17 2 ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v92 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (= v36 0 ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v95 (= v36 1 ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v95 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v97 (= v36 2 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v97 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v99 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v100 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v102 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v101 0 v0 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v101 v102 v0 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v99 v100 v103 ) ) )
[<-recv] success
[send->] (assert (= v106 (ite v99 0 v104 ) ) )
[<-recv] success
[send->] (assert (= v107 (+ v1 v105 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v107 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v106 ) )
[<-recv] success
[send->] (assert (= v109 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v110 (< v69 3 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v109 v110 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not v111 ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (assert (= v112 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v113 (= v69 1 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v115 (ite v112 (bytes-literal 1946528133 ) v114 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value1 ) )
[<-recv] success
[send->] (assert (= v4 txn_value1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v0 v1 ) ) )
[<-recv] success
[send->] (assert (= v6 (= v4 v5 ) ) )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value2 ) )
[<-recv] success
[send->] (assert (= v8 txn_value2 ) )
[<-recv] success
[send->] (assert (= v9 (= v8 v0 ) ) )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value3 ) )
[<-recv] success
[send->] (assert (= v27 txn_value3 ) )
[<-recv] success
[send->] (assert (= v28 (= v27 0 ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value4 ) )
[<-recv] success
[send->] (assert (= v41 txn_value4 ) )
[<-recv] success
[send->] (assert (= v42 (= v41 0 ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (assert (= v43 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v47 txn_value5 ) )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (assert (= v53 (= v25 v52 ) ) )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (assert (= v54 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v55 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (assert (= v57 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v58 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (assert (= v60 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (assert (= v64 (- 4 v36 ) ) )
[<-recv] success
[send->] (assert (= v65 (+ v17 v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
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
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v74 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v75 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
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
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
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
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v86 (= v17 0 ) ) )
[<-recv] success
[send->] (assert (= v87 (ite v86 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v87 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v88 (= v17 1 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v88 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v90 (= v17 2 ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v92 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (= v36 0 ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v95 (= v36 1 ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v95 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v97 (= v36 2 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v97 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v99 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v100 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v102 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v101 0 v0 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v101 v102 v0 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v99 v100 v103 ) ) )
[<-recv] success
[send->] (assert (= v106 (ite v99 0 v104 ) ) )
[<-recv] success
[send->] (assert (= v107 (+ v1 v105 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v107 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v106 ) )
[<-recv] success
[send->] (assert (= v109 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v110 (< v69 3 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v109 v110 false ) ) )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (assert (= v112 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v113 (= v69 1 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v115 (ite v112 (bytes-literal 1946528133 ) v114 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (= v3 (+ v0 v1 ) ) )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value1 ) )
[<-recv] success
[send->] (assert (= v4 txn_value1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v0 v1 ) ) )
[<-recv] success
[send->] (assert (= v6 (= v4 v5 ) ) )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value2 ) )
[<-recv] success
[send->] (assert (= v8 txn_value2 ) )
[<-recv] success
[send->] (assert (= v9 (= v8 v0 ) ) )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (assert (= v11 (= v10 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v12 (= v10 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v13 (= v10 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v14 (ite v12 true v13 ) ) )
[<-recv] success
[send->] (assert (= v15 (ite v11 true v14 ) ) )
[<-recv] success
[send->] (assert v15 )
[<-recv] success
[send->] (assert (= v16 (ite v12 1 2 ) ) )
[<-recv] success
[send->] (assert (= v17 (ite v11 0 v16 ) ) )
[<-recv] success
[send->] (assert (= v18 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v19 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v20 (ite v18 v19 false ) ) )
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
[send->] (assert (= v22 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v23 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v24 (msg-cat v22 v23 ) ) )
[<-recv] success
[send->] (assert (= v25 (digest v24 ) ) )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value3 ) )
[<-recv] success
[send->] (assert (= v27 txn_value3 ) )
[<-recv] success
[send->] (assert (= v28 (= v27 0 ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value4 ) )
[<-recv] success
[send->] (assert (= v41 txn_value4 ) )
[<-recv] success
[send->] (assert (= v42 (= v41 0 ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (assert (= v43 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v47 txn_value5 ) )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (assert (= v53 (= v25 v52 ) ) )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (assert (= v54 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v55 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (assert (= v57 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v58 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (assert (= v60 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (assert (= v64 (- 4 v36 ) ) )
[<-recv] success
[send->] (assert (= v65 (+ v17 v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
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
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v74 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v75 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
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
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
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
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v86 (= v17 0 ) ) )
[<-recv] success
[send->] (assert (= v87 (ite v86 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v87 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v88 (= v17 1 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v88 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v90 (= v17 2 ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v92 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (= v36 0 ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v95 (= v36 1 ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v95 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v97 (= v36 2 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v97 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v99 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v100 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v102 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v101 0 v0 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v101 v102 v0 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v99 v100 v103 ) ) )
[<-recv] success
[send->] (assert (= v106 (ite v99 0 v104 ) ) )
[<-recv] success
[send->] (assert (= v107 (+ v1 v105 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v107 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v106 ) )
[<-recv] success
[send->] (assert (= v109 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v110 (< v69 3 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v109 v110 false ) ) )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (assert (= v112 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v113 (= v69 1 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v115 (ite v112 (bytes-literal 1946528133 ) v114 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (define-fun ctc_balance0 () Int 0 )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (declare-fun txn_value1 () Int )
[<-recv] success
[send->] (define-fun ctc_balance1 () Int (+ ctc_balance0 txn_value1 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value1 ) )
[<-recv] success
[send->] (assert (= v4 txn_value1 ) )
[<-recv] success
[send->] (assert (= v5 (+ v0 v1 ) ) )
[<-recv] success
[send->] (assert (= v6 (= v4 v5 ) ) )
[<-recv] success
[send->] (assert v6 )
[<-recv] success
[send->] (declare-fun txn_value2 () Int )
[<-recv] success
[send->] (define-fun ctc_balance2 () Int (+ ctc_balance1 txn_value2 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value2 ) )
[<-recv] success
[send->] (assert (= v8 txn_value2 ) )
[<-recv] success
[send->] (assert (= v9 (= v8 v0 ) ) )
[<-recv] success
[send->] (assert v9 )
[<-recv] success
[send->] (declare-fun txn_value3 () Int )
[<-recv] success
[send->] (define-fun ctc_balance3 () Int (+ ctc_balance2 txn_value3 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value3 ) )
[<-recv] success
[send->] (assert (= v27 txn_value3 ) )
[<-recv] success
[send->] (assert (= v28 (= v27 0 ) ) )
[<-recv] success
[send->] (assert v28 )
[<-recv] success
[send->] (assert (= v30 (= v29 (bytes-literal 31217174 ) ) ) )
[<-recv] success
[send->] (assert (= v31 (= v29 (bytes-literal 3108938406 ) ) ) )
[<-recv] success
[send->] (assert (= v32 (= v29 (bytes-literal 1142816574 ) ) ) )
[<-recv] success
[send->] (assert (= v33 (ite v31 true v32 ) ) )
[<-recv] success
[send->] (assert (= v34 (ite v30 true v33 ) ) )
[<-recv] success
[send->] (assert v34 )
[<-recv] success
[send->] (assert (= v35 (ite v31 1 2 ) ) )
[<-recv] success
[send->] (assert (= v36 (ite v30 0 v35 ) ) )
[<-recv] success
[send->] (assert (= v37 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v38 (< v36 3 ) ) )
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
[send->] (declare-fun txn_value4 () Int )
[<-recv] success
[send->] (define-fun ctc_balance4 () Int (+ ctc_balance3 txn_value4 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value4 ) )
[<-recv] success
[send->] (assert (= v41 txn_value4 ) )
[<-recv] success
[send->] (assert (= v42 (= v41 0 ) ) )
[<-recv] success
[send->] (assert v42 )
[<-recv] success
[send->] (assert (= v43 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v44 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v45 (ite v43 v44 false ) ) )
[<-recv] success
[send->] (assert v45 )
[<-recv] success
[send->] (declare-fun txn_value5 () Int )
[<-recv] success
[send->] (define-fun ctc_balance5 () Int (+ ctc_balance4 txn_value5 ) )
[<-recv] success
[send->] (assert (<= 0 txn_value5 ) )
[<-recv] success
[send->] (assert (= v47 txn_value5 ) )
[<-recv] success
[send->] (assert (= v48 (= v47 0 ) ) )
[<-recv] success
[send->] (assert v48 )
[<-recv] success
[send->] (assert (= v49 (uint256->bytes v21 ) ) )
[<-recv] success
[send->] (assert (= v50 (uint256->bytes v17 ) ) )
[<-recv] success
[send->] (assert (= v51 (msg-cat v49 v50 ) ) )
[<-recv] success
[send->] (assert (= v52 (digest v51 ) ) )
[<-recv] success
[send->] (assert (= v53 (= v25 v52 ) ) )
[<-recv] success
[send->] (assert v53 )
[<-recv] success
[send->] (assert (= v54 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v55 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v56 (ite v54 v55 false ) ) )
[<-recv] success
[send->] (assert v56 )
[<-recv] success
[send->] (assert (= v57 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v58 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v59 (ite v57 v58 false ) ) )
[<-recv] success
[send->] (assert (= v60 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v61 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v62 (ite v60 v61 false ) ) )
[<-recv] success
[send->] (assert (= v63 (ite v59 v62 false ) ) )
[<-recv] success
[send->] (assert (= v64 (- 4 v36 ) ) )
[<-recv] success
[send->] (assert (= v65 (+ v17 v64 ) ) )
[<-recv] success
[send->] (assert (= v66 (mod v65 3 ) ) )
[<-recv] success
[send->] (assert (= v67 (ite v62 0 1 ) ) )
[<-recv] success
[send->] (assert (= v68 (ite v59 2 v67 ) ) )
[<-recv] success
[send->] (assert (= v69 (ite v63 v66 v68 ) ) )
[<-recv] success
[send->] (assert (= v70 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v71 (< v69 3 ) ) )
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
[send->] (assert (= v73 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v74 (<= 0 v17 ) ) )
[<-recv] success
[send->] (assert (= v75 (< v17 3 ) ) )
[<-recv] success
[send->] (assert (= v76 (ite v74 v75 false ) ) )
[<-recv] success
[send->] (assert (= v77 (ite v73 false true ) ) )
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
[send->] (assert (= v79 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v80 (<= 0 v36 ) ) )
[<-recv] success
[send->] (assert (= v81 (< v36 3 ) ) )
[<-recv] success
[send->] (assert (= v82 (ite v80 v81 false ) ) )
[<-recv] success
[send->] (assert (= v83 (ite v79 false true ) ) )
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
[send->] (assert (= v85 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v86 (= v17 0 ) ) )
[<-recv] success
[send->] (assert (= v87 (ite v86 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v87 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v88 (= v17 1 ) ) )
[<-recv] success
[send->] (assert (= v89 (ite v88 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v89 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v90 (= v17 2 ) ) )
[<-recv] success
[send->] (assert (= v91 (ite v90 v85 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v91 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v92 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v93 (= v36 0 ) ) )
[<-recv] success
[send->] (assert (= v94 (ite v93 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v94 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v95 (= v36 1 ) ) )
[<-recv] success
[send->] (assert (= v96 (ite v95 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v96 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v97 (= v36 2 ) ) )
[<-recv] success
[send->] (assert (= v98 (ite v97 v92 false ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert v98 )
[<-recv] success
[send->] (check-sat )
[<-recv] sat
[send->] (pop 1 )
[<-recv] success
[send->] (assert (= v99 (= v69 2 ) ) )
[<-recv] success
[send->] (assert (= v100 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v101 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v102 (* 2 v0 ) ) )
[<-recv] success
[send->] (assert (= v103 (ite v101 0 v0 ) ) )
[<-recv] success
[send->] (assert (= v104 (ite v101 v102 v0 ) ) )
[<-recv] success
[send->] (assert (= v105 (ite v99 v100 v103 ) ) )
[<-recv] success
[send->] (assert (= v106 (ite v99 0 v104 ) ) )
[<-recv] success
[send->] (assert (= v107 (+ v1 v105 ) ) )
[<-recv] success
[send->] (define-fun ctc_balance6 () Int (- ctc_balance5 v107 ) )
[<-recv] success
[send->] (define-fun ctc_balance7 () Int (- ctc_balance6 v106 ) )
[<-recv] success
[send->] (assert (= v109 (<= 0 v69 ) ) )
[<-recv] success
[send->] (assert (= v110 (< v69 3 ) ) )
[<-recv] success
[send->] (assert (= v111 (ite v109 v110 false ) ) )
[<-recv] success
[send->] (assert v111 )
[<-recv] success
[send->] (assert (= v112 (= v69 0 ) ) )
[<-recv] success
[send->] (assert (= v113 (= v69 1 ) ) )
[<-recv] success
[send->] (assert (= v114 (ite v113 (bytes-literal 3502248497 ) (bytes-literal 1195058296 ) ) ) )
[<-recv] success
[send->] (assert (= v115 (ite v112 (bytes-literal 1946528133 ) v114 ) ) )
[<-recv] success
[send->] (push 1 )
[<-recv] success
[send->] (assert (not (= ctc_balance7 0 ) ) )
[<-recv] success
[send->] (check-sat )
[<-recv] unsat
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (pop 1 )
[<-recv] success
[send->] (exit )
