#lang racket

; GC mechanism's utilities

(require redex
         "../grammar.rkt"
         "./weakTables.rkt"
         )

; definition of setFin, figure 6: codifies the logic behind marking an object
; for finalization
; PRE : {tid_1, tid_2 ∈ dom(θ)}
(define-metafunction ext-lang
  setFin : tid v θ -> pos

  ; π 3 (θ(tid)) == ⊘
  [(setFin tid _ (_ ... (tid (_ _ ⊘)) _ ...))
   ⊘]

  ; {π 3 (θ(tid_1)) != ⊘}
  [(setFin tid nil _)
   ⊥]
  
  [(setFin tid_1 tid_2 (_ ...
                        (tid_1 (_ tid_2 pos))
                        _ ...))
   pos]

  ; {π 2 (θ(tid_1)) != tid_2}
  ; unmark for finalization
  [(setFin tid_1 tid_2 (_ ...
                        (tid_2 (tableconstructor _ _))
                        _ ...))
   ⊥
   
   (side-condition
    (not (redex-match? ext-lang
                       (\{ _ ... (\[ "__gc" \] = _) _ ... \})
                       (term tableconstructor))))]

  ; {{π 3 (θ(tid_1)) != ⊘ ∧ π 2 (θ(tid_1)) != tid_2 ∧ “ gc” ∈ π 1 (θ(tid_2))}
  [(setFin tid_1 _ θ)
   ,(+ 1 (term (maxPos θ)))
   ]
  )

(provide setFin)

; auxiliary function of setFin: returns the max. priority for finalization among
; tables marked
(define-metafunction ext-lang
  maxPos : θ -> number
  
  [(maxPos θ)
   (maxPosAux θ 0)]
  )

(define-metafunction ext-lang
  maxPosAux : θ number -> number
  
  [(maxPosAux () number)
   number]

  [(maxPosAux (osp ... (tid (_ _ number_1))) number_2)
   (maxPosAux (osp ...) number_1)

   (side-condition (> (term number_1) (term number_2)))]
  
  [(maxPosAux (osp ... (tid (_ _ _))) number)
   ; {number >= pos or pos ∉ number}
   (maxPosAux (osp ...) number)]

  ; discard fdef
  [(maxPosAux (osp ... (cid _)) number)
   (maxPosAux (osp ...) number)]
  )

; checks if a table does not appear as a weak value or is reachable from a
; weak value
(define-metafunction ext-lang
  notFinVal : tid σ θ -> any

  ; ∃ tid_2 with weak values / tid_1 is reachable from a value in tid_2
  [(notFinVal tid_1 σ θ)
   #f

   (where (side-condition
               (tid_1 σ
                      (osp_1 ...
                (tid_2 ((\{ efield_1 ... (\[ v_1 \] = v_2)
                            efield_2 ... \})
                        v_3 pos))
                osp_2 ...))
               (and (term (wv? tid_2 (osp_1 ...
                                      (tid_2 ((\{ efield_1 ...
                                                  (\[ v_1 \] = v_2)
                                                  efield_2 ... \})
                                              v_3 pos))
                                      osp_2 ...)))
                    (term (reach tid_1 v_2 σ
                                 (osp_1 ...
                                  (tid_2 ((\{ efield_1 ...
                                              (\[ v_1 \] = v_2)
                                              efield_2 ... \})
                                          v_3 pos))
                                  osp_2 ...)))))
          (tid_1 σ θ))]

  ; default
  [(notFinVal _ _ _)
   #t]
  )

; to test notFinVal in another module
(provide notFinVal)

; stop-the-world GC algorithm that removes any garbage in σ, as defined by
; reach, and taking into account dangling pointer errors related with
; finalization
; RETURNS: a subset of σ which does not contain garbage
(define-metafunction ext-lang
  cleanSigma : σ θ s -> σ
  
  [(cleanSigma σ θ s)
   (cleanSigmaAux σ θ s σ)])

(provide cleanSigma)

(define-metafunction ext-lang
  cleanSigmaAux : σ θ s σ -> σ
  
  [(cleanSigmaAux σ θ s ())
   ()]

  ; hack to maintain stdout in σ
  [(cleanSigmaAux σ θ s ((refStdout v) vsp ...))
   ,(append (term ((refStdout v)))
            (term (cleanSigmaAux σ θ s (vsp ...))))]

  [(cleanSigmaAux σ θ s ((r v) vsp ...))
   ,(append (term ((r v)))
            (term (cleanSigmaAux σ θ s (vsp ...))))

   (side-condition (or (term (reach r s σ θ))
                       (not (term (notReachFin r σ θ)))))
   ]

  ;{¬ reach r s σ θ ∧ notReachFin r σ θ}
  [(cleanSigmaAux σ θ s (vsp_1 vsp_2 ...))
   (cleanSigmaAux σ θ s (vsp_2 ...))]
  )

; stop-the-world gc algorithm that cleans the given θ store and returns the
; new θ store and the next object to be finalized, if any; garbage is defined
; in terms of reachCte (it doesn't include cleaning of weak tables)
; PARAMS:
; - σ θ s: original configuration
;
; RETURNS:
; - new θ store
; - tid of next table to be finalized or nil
(define-metafunction ext-lang
  
  cleanThetaWeak : σ θ s -> any

  [(cleanThetaWeak σ θ s)
   (cleanThetaWeakAux σ θ s θ 0 nil)]
  )

(provide cleanThetaWeak)

(define-metafunction ext-lang
  ; domain:
  ; - original σ
  ; - original θ
  ; - term s from which the root set is computed
  ; - instance of θ upon which recursion is done
  ; - highest priority for finalization found so far
  ; - tid of the table with the highest priority for finalization found, or nil
  
  ; returns:
  ; - new θ store
  ; - tid of next table to be finalized, or nil
  cleanThetaWeakAux : σ θ s θ Number v -> (θ v)
  
  ; base case
  [(cleanThetaWeakAux σ θ s () Number v)
   (() v)]

  ; objid is reachable
  [(cleanThetaWeakAux σ θ s ((objid any_1) osp_1 ... )
                      Number v_1)
   (((objid any_1) osp_4 ...) v_2)

   (where (osp_2 ... (objid any_1) osp_3 ...) θ)
   (where #t (reachCte objid s σ (osp_2 ... osp_3 ...)))
   
   ; clean the tail of the store
   (where ((osp_4 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number v_1))]

  ; {objid is not reachCte}

  ; check if objid is a table, marked for finalization, with the highest
  ; priority found so far and if it does not appear as a weak value
  [(cleanThetaWeakAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...) Number_2 v_1)
   (((tid (any_1 any_2 Number_1)) osp_2 ...) v_2)

   ; tid is set for finalization, check if its priority is the highest found so
   ; far (for performance, we compute here predicate next_fin, from fig. 8, as
   ; recursion is done)
   (side-condition (> (term Number_1)
                      (term Number_2)))

   ; check that tid does not appear as a weak value or is reachable from a weak
   ; value (otherwise, it must be cleaned from the table)
   (where #t (notFinVal tid σ θ))
   
   ; {tid does not appear as a value from a weak table} => it can be finalized

   ; clean the tail of the store; take tid into account for finalization
   (where ((osp_2 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number_1 tid))]

  ; {(objid is a non-reachCte table ∧
  ;   (objid is not set for finalization ∨
  ;    it does not have the highest priority found so far ∨
  ;    tid appears as a weak value of another table)
  ;
  ;   ∨
  ;
  ;   it's a non-reachCte closure}
  
  ; check if objid is a table, marked for finalization or reachable from a
  ; table marked for finalization
  [(cleanThetaWeakAux σ θ s ((objid any_1) osp_1 ...) Number v_1)
   (((objid any_1) osp_2 ...) v_2)

   (where #f (notReachFin objid σ θ))

   ; {objid is reachable from a table (or its metatable) marked for
   ; finalization}
   (where ((osp_2 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number v_1))]

  ; {((objid is a non-reachCte table ∧ not set for finalization)
  ;   ∨
  ;   it's a non-reachCte closure)
  ;   ∧
  ;   it is not reachable from a table marked for finalization}
  [(cleanThetaWeakAux σ θ s ((objid any_1) osp_1 ... )
                      Number v_1)
   (((objid any_1) osp_4 ...) v_2)

   ; if objid is reachable, but not reachCte, it means that some weak table
   ; has a weak ref to the object; we need, first, to remove the ref from the
   ; weak table, and then perform its cleaning here
   (where (osp_2 ... (objid any_1) osp_3 ...) θ)
   (where #t (reach objid s σ (osp_2 ... osp_3 ...)))
   
   ; clean the tail of the store
   (where ((osp_4 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number v_1))]

  ; {ops_1 is not reachable}
  ; osp_1 can be removed
  [(cleanThetaWeakAux σ θ s (osp_1 osp_2 ... ) Number v)
   (cleanThetaWeakAux σ θ s (osp_2 ... ) Number v)]
  )

; given the next invocation of a finalizer, cid (tid), sets ⊘ in tid, to
; prevent it from being marked again for finalization
(define-metafunction ext-lang
  
  preventFin : e θ -> θ

  ; finalizer are called in protected mode
  [(preventFin (cid (tid)) (osp_1 ...
                                           (tid (evaluatedtable v pos))
                                           osp_2 ...))
   (osp_1 ...
    (tid (evaluatedtable v ⊘))
    osp_2 ...)]

  [(preventFin nil θ)
   θ])

(provide preventFin)

; looks for the finalizer of a given tid, returns an invocation to it (as a term
; from ext-lang), or nil, if no finalizer is found or no tid is given
(define-metafunction ext-lang
  invokeFin : e θ -> e

  ; table set for finalization, finalizer provided
  [(invokeFin tid_1 (osp_1 ... (tid_1 (evaluatedtable tid_2 pos)) osp_2 ...))
   (cid (tid_1))

   ; extract finalizer
   (where (_ ... (tid_2 ((\{ _ ...
                             (\[ "__gc" \] = cid)
                             _ ... \}) _ _)) _ ...)
          (osp_1 ... (tid_1 (evaluatedtable tid_2 pos)) osp_2 ...))]

  ; no tid for finalization provided, or no finalizer set
  [(invokeFin _ _)
   nil])

(provide invokeFin)