#lang racket

; GC mechanism's utilities

(require redex
         "../grammar.rkt"
         "./objStoreMetaFunctions.rkt"
         "./valStoreMetaFunctions.rkt"
         )
;                                                                                   
;                                                                                   
;                                                                                   
;               ;                      ;;;;                                         
;               ;                         ;                                         
;                                         ;                                         
;    ;;;;;    ;;;     ;;;;;;   ; ;;;      ;        ;;;               ;;; ;    ;;;   
;   ;     ;     ;     ;  ;  ;  ;;   ;     ;       ;   ;             ;   ;;   ;   ;  
;   ;           ;     ;  ;  ;  ;     ;    ;      ;     ;           ;     ;  ;       
;   ;;;;        ;     ;  ;  ;  ;     ;    ;      ;     ;           ;     ;  ;       
;       ;;;     ;     ;  ;  ;  ;     ;    ;      ;;;;;;;           ;     ;  ;       
;         ;     ;     ;  ;  ;  ;     ;    ;      ;                 ;     ;  ;       
;   ;     ;     ;     ;  ;  ;  ;;   ;     ;       ;    ;            ;   ;;   ;   ;  
;    ;;;;;   ;;;;;;;  ;  ;  ;  ; ;;;       ;;;     ;;;;              ;;; ;    ;;;   
;                              ;                                         ;          
;                              ;                                    ;   ;;          
;                              ;                                     ;;;;           
;
; definition 3.1 (Reachability for Simple GC)
; domain is (e t σ θ) instead of e x σ x θ, to put the whole tuple into the
; pattern in side-condition
(define-metafunction ext-lang
  reach : (e (side-condition any (is_term? (term any))) σ θ) -> any

  ; location belongs to the root set
  [(reach (e_1 any σ θ))
   #t
   
   (side-condition (redex-match? ext-lang
                                 (side-condition (in-hole C e_2)
                                                 (equal? (term e_1)
                                                         (term e_2)))
                                 (term any)))
   ]

  ; {e_1 ∉ any_1}
  [(reach (side-condition (e (in-hole C r) (vsp_1 ... (r v) vsp_2 ...) θ)
                          (term (reach (e v (vsp_1 ... vsp_2 ...) θ)))))
   #t]

  [(reach (side-condition (e (in-hole C tid) σ
                             (osp_1 ... (tid (evaluatedtable any _)) osp_2 ...))
                          (or (term (reach (e evaluatedtable σ
                                              (osp_1 ... osp_2 ...))))
                              (term (reach (e any σ
                                              (osp_1 ... osp_2 ...)))))))
   #t]

  [(reach (side-condition (e (in-hole C cid) σ
                             (osp_1 ... (cid functiondef) osp_2 ...))
                          (term (reach (e functiondef σ
                                          (osp_1 ... osp_2 ...))))))
   #t]

  [(reach _)
   #f]
  )

(provide reach)


;                                                                                                              
;                                                                                                              
;                                                                                                              
;       ;;;     ;                      ;;;;         ;                                   ;                      
;      ;        ;                         ;         ;                         ;         ;                      
;      ;                                  ;                                   ;                                
;    ;;;;;;   ;;;     ; ;;;;     ;;;;     ;       ;;;     ;;;;;;;    ;;;;   ;;;;;;    ;;;       ;;;    ; ;;;;  
;      ;        ;     ;;   ;;   ;    ;    ;         ;           ;   ;    ;    ;         ;      ;   ;   ;;   ;; 
;      ;        ;     ;     ;        ;    ;         ;          ;         ;    ;         ;     ;     ;  ;     ; 
;      ;        ;     ;     ;   ;;;;;;    ;         ;         ;     ;;;;;;    ;         ;     ;     ;  ;     ; 
;      ;        ;     ;     ;  ;;    ;    ;         ;       ;;     ;;    ;    ;         ;     ;     ;  ;     ; 
;      ;        ;     ;     ;  ;     ;    ;         ;      ;       ;     ;    ;         ;     ;     ;  ;     ; 
;      ;        ;     ;     ;  ;    ;;    ;         ;     ;        ;    ;;    ;         ;      ;   ;   ;     ; 
;      ;     ;;;;;;;  ;     ;   ;;;; ;     ;;;   ;;;;;;;  ;;;;;;;   ;;;; ;     ;;;   ;;;;;;;    ;;;    ;     ; 
;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                                                                                              
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

; determines if a given tid is marked for finalization
(define-metafunction ext-lang
  marked : tid θ -> any

  [(marked tid (_ ... (tid (_ _ number)) _ ...))
   #t]

  ; default case
  [(marked _ _)
   #f]
  )

(provide marked)

; checks if a given location is reachable from a table marked for finalization
(define-metafunction ext-lang
  notReachFin : (e σ θ) -> any
  ; domain is (e σ θ) instead of e x σ x θ, to put the whole tuple into the
  ; pattern in side-condition
  [(notReachFin (side-condition
                 (e σ (osp_1 ... (tid any) osp_2 ...))
                 (and (not (equal? (term tid)
                                   (term e)))
                      (term (marked tid (osp_1 ...
                                         (tid any)
                                         osp_2 ...)))
                      (term (reach (e tid σ
                                      (osp_1 ... (tid any) osp_2 ...)))))))
   #f]

  ; default case
  [(notReachFin _)
   #t]
  )

(provide notReachFin)

; predicate fin, figure 8: determines if a given tid is in condition to be
; finalized: not reachable and marked for finalization
(define-metafunction ext-lang
  fin : tid s σ θ -> any

  [(fin tid s σ θ)
   #t

   ; to avoid escaping to racket
   (where #f (reach (tid s σ θ)))

   (where #t (marked tid θ))]

  ; default
  [(fin _ _ _ _)
   #f]
  )

(provide fin)

; auxiliar function to simplify obtention of pos
(define-metafunction ext-lang
  getPos : tid θ -> pos

  [(getPos tid (osp_1 ... (tid (_ _ number)) osp_2 ...))
   number]

  ; default
  [(getPos _ _)
   0])

; predicate next_fin, figure 8
; compares a given tid's finalization pos, with any other table marked for
; finalization (regardless the value of fin(tid)); returns #t if it's the next
; table to be finalized
(define-metafunction ext-lang
  nextFin : (tid s σ θ) -> any

  ; ∃ tid_2, fin(tid_2,...) ∧ pos_tid_2 > pos_tid_1
  [(nextFin (side-condition
             (tid_1 s σ (osp_1 ...
                         (tid_2 (evaluatedtable any pos_1))
                         osp_2 ...))
             (and (term (fin tid_2 s σ (osp_1 ...
                                        (tid_2 (evaluatedtable any pos_1))
                                        osp_2 ...)))
                  (> (term pos_1)
                     (term (getPos tid_1
                                   (osp_1 ...
                                    (tid_2 (evaluatedtable any pos_1))
                                    osp_2 ...)))))))
   #f]

  ; default
  [(nextFin _)
   #t]
  )

(provide nextFin)

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

   (side-condition (or (term (reach (r s σ θ)))
                       (not (term (notReachFin (r σ θ))))))
   ]

  ;{¬ reach r s σ θ ∧ ¬ notReachFin r σ θ}
  [(cleanSigmaAux σ θ s (vsp_1 vsp_2 ...))
   (cleanSigmaAux σ θ s (vsp_2 ...))]
  )

; stop-the-world gc algorithm that cleans the given θ store and returns the
; next object to be finalized, if any. Garbage is defined in terms of reach
(define-metafunction ext-lang
  cleanTheta : σ θ s -> any

  [(cleanTheta σ θ s)
   (cleanThetaAux σ θ s θ 0 nil)]
  )

(provide cleanTheta)

(define-metafunction ext-lang
  ; σ x θ x s x θ x highest priority found x tid of table marked for fin 
  cleanThetaAux : σ θ s θ Number v -> (θ v)
  
  ; base case
  [(cleanThetaAux σ θ s () Number v)
   (() v)]

  ; objid is reachable
  [(cleanThetaAux σ θ s ((objid any_1) osp_1 ... )
                  Number v)
   (((objid any_1) osp_2 ...) any_2)

   (side-condition (term (reach (objid s σ θ))))

   ; clean the tail of the store
   (where ((osp_2 ...) any_2) (cleanThetaAux σ θ s (osp_1 ... ) Number v))]

  ; {objid is not reachable}
  ; finalizer set, is not the highest priority found so far
  [(cleanThetaAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...)
                  Number_2 v)
   (((tid (any_1 any_2 Number_1)) osp_2 ...) any_3)
   
   (side-condition (<= (term Number_1)
                       (term Number_2)))

   ; clean the tail of the store
   (where ((osp_2 ...) any_3) (cleanThetaAux σ θ s (osp_1 ... ) Number_2 v))]

  ; {(tid is not reachable and (tid is not set for finalization ∨ has the highest
  ; priority found so far)) ∨ it's a non-reachable closure}
  ; tid is set for finalization: must be the next one
  [(cleanThetaAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...) Number_2 v)
   (((tid (any_1 any_2 Number_1)) osp_2 ...) any_3)

   (side-condition (> (term Number_1)
                      (term Number_2)))
   
   ; clean the tail of the store
   (where ((osp_2 ...) any_3) (cleanThetaAux σ θ s (osp_1 ... ) Number_1 tid))
   ]

  ; {(tid is not reachable and tid is not set for finalization) ∨
  ;  it's a non-reachable closure}
  ; tid is reachable from a table (or its metatable) which is marked for
  ; finalization: must not be removed
  [(cleanThetaAux σ θ s ((tid any_1) osp_1 ...) Number v)
   (((tid any_1) osp_2 ...) any_2)

   (side-condition (not (term (notReachFin (tid σ θ)))))
   
   (where ((osp_2 ...) any_2) (cleanThetaAux σ θ s (osp_1 ... ) Number v))]

  ; {(tid is not reachable ∧ tid is not set for finalization ∧ tid does not
  ; appear into a metatable) ∨ it's a non-reachable closure}
  [(cleanThetaAux σ θ s (osp_1 osp_2 ... ) Number v)
   (cleanThetaAux σ θ s (osp_2 ... ) Number v)]
  )

; garbage collection aware of the finalization
(define-metafunction ext-lang
  gcFin : s σ θ -> (σ θ e)

  ; no finalization
  [(gcFin s σ_1 θ_1)
   (σ_2 θ_2 nil)

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 0) (cleanTheta σ_1 θ_1 s))
   ]

  ; finalization: set the table to ⊘
  [(gcFin s σ_1 θ_1)
   (σ_2 (osp_1 ... (tid_1 (evaluatedtable tid_2 ⊘)) osp_2 ...) (cid (tid_1)))

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 tid_1) (cleanTheta σ_1 θ_1 s))

   ; obtain metatable
   (where (osp_1 ... (tid_1 (evaluatedtable tid_2 pos)) osp_2 ...) θ_2)

   ; extract finalizer
   (where (_ ... (tid_2 ((\{ _ ...
                             (\[ "__gc" \] = cid)
                             _ ... \}) _ _)) _ ...) θ_2)
   ]

  ; table set for finalization, no finalizer provided
  [(gcFin s σ_1 θ_1)
   (σ_2 θ_2 nil)

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 tid_1) (cleanTheta σ_1 θ_1 s))
   ]
  )

(provide gcFin)

;                                                                                          
;                                                                                          
;                           ;                               ;       ;;;                    
;                           ;                               ;         ;                    
;                           ;                 ;             ;         ;                    
;                           ;                 ;             ;         ;                    
;  ;      ;  ;;;;     ;;;   ;   ;           ;;;;;;    ;;;   ;;;;;     ;      ;;;;    ;;;;  
;  ;      ; ;;  ;;   ;   ;  ;  ;              ;      ;   ;  ;;  ;;    ;     ;;  ;;  ;    ; 
;   ; ;; ;  ;    ;       ;  ; ;               ;          ;  ;    ;    ;     ;    ;  ;      
;   ; ;; ;  ;;;;;;   ;;;;;  ;;;               ;      ;;;;;  ;    ;    ;     ;;;;;;   ;;;;  
;   ; ;; ;  ;       ;    ;  ;  ;              ;     ;    ;  ;    ;    ;     ;            ; 
;    ;  ;   ;;   ;  ;   ;;  ;   ;             ;     ;   ;;  ;;  ;;    ;     ;;   ;  ;    ; 
;    ;  ;    ;;;;    ;;; ;  ;    ;             ;;;   ;;; ;  ;;;;;      ;;;   ;;;;    ;;;;  
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

; checks if tid has weak keys
(define-metafunction ext-lang
  wk? : tid θ -> any

  [(wk? tid_1 θ)
   ,(string-contains? (term String) "k")

   ; dereference tid_1, extract metatable
   (where (_ ... (tid_1 (_ tid_2 _)) _ ...)
          θ)

   ; index metatable with "__mode"
   (where (_ ...
           (tid_2 ((\{ _ ... (\[ "__mode" \] = String) _ ... \}) _ _))
           _ ...)
          θ)]

  [(wk? tid θ)
   #f])

(provide wk?)

; checks if tid has weak values
(define-metafunction ext-lang
  wv? : tid θ -> any

  [(wv? tid_1 θ)
   ,(string-contains? (term String) "v")

   ; dereference tid_1, extract metatable
   (where (_ ... (tid_1 (_ tid_2 _)) _ ...)
          θ)

   ; index metatable with "__mode"
   (where (_ ...
           (tid_2 ((\{ _ ... (\[ "__mode" \] = String) _ ... \}) _ _))
           _ ...)
          θ)]

  [(wv? objref θ)
   #f])

(provide wv?)

; function SO, figure 10: members of tid referenced by strong references
(define-metafunction ext-lang
  SO : tid θ -> (any ...)

  ; wv and not wk
  [(SO tid θ)
   any

   (side-condition (and (term (wv? tid θ))
                        (not (term (wk? tid θ)))))

   (where (_ ... (tid ((\{ efield ... \}) _ _)) _ ...) θ)

   ; only the keys are referred by strong references
   (where any (cteKeys (efield ...)))]

  ; not wv and not wk
  [(SO tid θ)
   any

   (side-condition (and (not (term (wv? tid θ)))
                        (not (term (wk? tid θ)))))
   
   (where (_ ... (tid ((\{ efield ... \}) _ _)) _ ...) θ)

   ; keys and values are referred by strong references
   (where any (cteFields (efield ...)))]

  ; not wv and wk
  [(SO tid θ)
   any

   (side-condition (and (not (term (wv? tid θ)))
                        (term (wk? tid θ))))
   
   (where (_ ... (tid ((\{ efield ... \}) _ _)) _ ...) θ)

   ; ephemeron table
   (where any (cteEphemeron (efield ...)))]

  ; wv and wk
  [(SO _ _)
   ()]
  )

(provide SO)

; auxiliary function of SO: returns ctes in keys (case weak table with weak
; values)
(define-metafunction ext-lang
  cteKeys : (field ...) -> (e ...)

  [(cteKeys ())
   ()]
  
  [(cteKeys ((\[ cte \] = _) field ...))
   ,(append (term (cte))
            (term (cteKeys (field ...))))]

  [(cteKeys (_ field ...))
   (cteKeys (field ...))]
  )

; auxiliary function of SO: returns ctes in values and keys (case non weak
; table)
(define-metafunction ext-lang
  cteFields : (field ...) -> (e ...)

  [(cteFields ())
   ()]
  
  [(cteFields ((\[ cte_1 \] = cte_2) field ...))
   ,(append (term (cte_1 cte_2))
            (term (cteFields (field ...))))]

  [(cteFields ((\[ cte \] = v) field ...))
   ,(append (term (cte))
            (term (cteFields (field ...))))]

  [(cteFields ((\[ v \] = cte) field ...))
   ,(append (term (cte))
            (term (cteFields (field ...))))]

  ; default case: no CTEs in field
  [(cteFields (_ field ...))
   (cteFields (field ...))]
  )

; auxiliary function of SO: returns ctes according to the semantics of
; ephemerons
(define-metafunction ext-lang
  cteEphemeron : (field ...) -> ((e e) ...)

  [(cteEphemeron ())
   ()]
  
  [(cteEphemeron ((\[ v \] = cte) field ...))
   ,(append (term ((v cte)))
            (term (cteEphemeron (field ...))))]

  ; default case: no CTEs in field
  [(cteEphemeron (_ field ...))
   (cteEphemeron (field ...))]
  )

(provide cteEphemeron)

; predicate eph from figure 11: determines if a given objid is reachable from
; a given (k,v) pair from an ephemeron tid (it only takes into account
; reachablity from v, provided k is reachable)
; PRE : {(v cte) ∈ π_1(θ(tid))}
(define-metafunction ext-lang
  eph : objid (v cte) tid (side-condition any (is_term? (term any))) σ θ -> any
  [(eph objid (cte_1 cte_2) tid any_1 σ (osp_1 ...
                                         (tid ((\{ efield_1 ...
                                                   (\[ cte_1 \] = cte_2)
                                                   efield_2 ... \}) any_2
                                                                    any_3))
                                         osp_2 ...))
   #t

   (side-condition (and (term (reachCte (cte_1 any_1 σ
                                               (osp_1 ...
                                                (tid ((\{ efield_1 ...
                                                          efield_2 ... \})
                                                      any_2
                                                      any_3))
                                                osp_2 ...)
                                               any_1)))
                        
                        (term (reachCte (objid cte_2 σ
                                               (osp_1 ...
                                                (tid ((\{ efield_1 ...
                                                          efield_2 ... \})
                                                      any_2
                                                      any_3))
                                                osp_2 ...)
                                               any_1)))))]

  ; {¬ (reachCte cte_1 any_1 σ ...) ∨ ¬ (reachCte objid cte_2 ...)}
  [(eph objid (cte_1 cte_2) tid any_1 σ θ)
   #f]

  ; {k ∉ cte}
  [(eph objid (v cte) tid any σ (osp_1 ...
                                 (tid ((\{ efield_1 ...
                                           (\[ v \] = cte)
                                           efield_2 ... \})
                                       any_2
                                       any_3))
                                 osp_2 ...))
   #t

   (side-condition (term (reachCte (objid cte σ
                                          (osp_1 ...
                                           (tid ((\{ efield_1 ...
                                                     efield_2 ... \})
                                                 any_2
                                                 any_3))
                                           osp_2 ...) any))))]

  ; {¬ (reachCte objid cte σ θ any)}
  [(eph objid (v cte) tid any σ θ)
   #f]
  )

; auxiliary predicate of reachTable, figure 11
; performs iteration over the elements from SO (tid)
(define-metafunction ext-lang
  reachTableAux : (objid tid σ θ any (any ...)) -> any

  ; cannot delete table tid: we still need to look for its keys, in case
  ; the reachability of some key comes from another value of the same table
  [(reachTableAux (side-condition (objid tid σ θ any 
                                         (any_1 ... (e_1 e_2) any_2 ...))
                                  (term (eph objid (e_1 e_2) tid any σ θ))))
   #t
   ]

  ; we can delete the binding of tid
  [(reachTableAux (side-condition (objid tid σ (osp_1 ... (tid _) osp_2 ...)
                                         any
                                         (e_1 ... e e_2 ...))
                                  (term (reachCte (objid e σ
                                                         (osp_1 ... osp_2 ...)
                                                         any)))))
   #t
   ]

  ; default case
  [(reachTableAux _)
   #f
   ]
  )

; predicate reachTable, figure 11: determines if a given objid is reachable
; from a table tid, taking into account the semantics of weak tables
(define-metafunction ext-lang
  reachTable : objid tid σ θ (side-condition any (is_term? (term any))) -> any

  [(reachTable objid tid σ θ any_2)
   #t

   (side-condition (term (reachTableAux (objid tid σ θ any_2 (SO tid θ)))))
   ]

  ; reach from metatable
  [(reachTable objid tid σ (osp_1 ... (tid (_ any_1 _)) osp_2 ...) any_2)
   #t

   ; we need to remove tid from theta
   (side-condition (term (reachCte (objid any_1 σ
                                          (osp_1 ... osp_2 ...)
                                          any_2))))
   ]

  ; default case
  [(reachTable _ _ _ _ _)
   #f
   ]
  )

(provide reachTable)

; predicate reachCte, figure 11: determines if a given cte is reachable, from
; a given term that defines the root set, and taking into account the semantics
; of weak tables
(define-metafunction ext-lang
  ; domain:
  ; id of cte x
  ; term t from which the reachability path must pass x
  ; original σ x
  ; original θ x
  ; term t from which the original root set is computed (needed for predicate
  ; eph in reachTable)
  reachCte : (objid t σ θ t) -> any

  ; objid ∈ root set
  [(reachCte (objid (in-hole C objid) _ _ _))
   #t
   ]

  ; path through a r ∈ dom(σ)
  [(reachCte (side-condition
              (objid (in-hole C r) (vsp_1 ... (r v) vsp_2 ...) θ t)
              (term (reachCte (objid v (vsp_1 ... vsp_2 ...) θ t)))))
   #t
   ]

  ; path through a tid ∈ dom(θ)
  [(reachCte (side-condition
              (objid (in-hole C tid) σ (osp_1 ... (tid any_2) osp_2 ...) t)
              (term (reachTable objid tid σ
                                (osp_1 ... (tid any_2) osp_2 ...)
                                t))))
   #t
   ]

  ; path through a cid ∈ dom(θ)
  [(reachCte (side-condition
              (objid (in-hole C cid) σ (osp_1 ... (cid functiondef) osp_2 ...)
                     t)
              (term (reachCte (objid functiondef σ
                                     (osp_1 ... osp_2 ...)
                                     t)))))
   #t
   ]

  ; default
  [(reachCte _)
   #f
   ]
  )

(provide reachCte)

; implements the removal of fields of a given weak table, as specified in
; figure 12

; PARAMS:
; original σ store
; original θ store
; term s from which the original root set is compued
; tid of the table being cleaned
; fields of the table being cleaned

; PRE : {(field ...) belongs to a weak table (condition "weakness")}
(define-metafunction ext-lang
  cleanWeakTable : σ θ s tid (field ...) -> (field ...)

  [(cleanWeakTable σ θ s tid ())
   ()]

  ; field not cleaned
  [(cleanWeakTable σ θ s tid ((\[ v_1 \] = v_2) field_1 ...))
   ((\[ v_1 \] = v_2) field_2 ...)

   ; condition weakness is assumed (given the actual PRE)

   ; reachable
   (side-condition (and (or (not (is_cte? (term v_1)))
                            (term (reachCte (v_1 s σ θ s))))
                        (or (not (is_cte? (term v_2)))
                            (term (reachCte (v_2 s σ θ s))))))

   (where (field_2 ...) (cleanWeakTable σ θ s tid (field_1 ...)))
   ]

  ; {field element not reachable or cte}
  [(cleanWeakTable σ θ s tid ((\[ v_1 \] = v_2) field_1 ...))
   (field_2 ...)

   ; condition fin_key: if v_1 is a table marked for finalization, it should not
   ; be removed from weak keys before being finalized
   (side-condition (or (not (and (term (wk? tid θ))
                                 (is_tid? (term v_1))))
                        (term (notResWeakKey (v_1 σ θ s)))))
   
   (where (field_2 ...) (cleanWeakTable σ θ s tid (field_1 ...)))
   ]

  ; {key or value not reachable ∧ ¬ fin key}
  [(cleanWeakTable σ θ s tid ((\[ v_1 \] = v_2) field_1 ...))
   ; {key or value not reachable ∧ ¬ fin key}
   ((\[ v_1 \] = v_2) field_2 ...)   

   (where (field_2 ...) (cleanWeakTable σ θ s tid (field_1 ...)))
   ]
  ) 

; PARAM : θ, the actual object store, with every unreachable object cleaned
; PARAM : (objref ...), the reacheable portion of θ: dom(θ) = (objref ...)
; RETURNS : the object store with all its weak tables cleaned

(define-metafunction ext-lang
  cleanWeakTables : σ θ s -> θ

  [(cleanWeakTables σ θ s)
   (cleanWeakTablesAux σ θ s θ)]
  )

(provide cleanWeakTables)

(define-metafunction ext-lang
  cleanWeakTablesAux : σ θ s θ -> θ

  [(cleanWeakTablesAux σ θ s ())
   ()]

  ; Weak table
  [(cleanWeakTablesAux σ θ s ((tid ((\{ field_1 ... \}) any_1 any_2))
                              osp_1 ...))
   ((tid ((\{ field_2 ... \}) any_1 any_2)) osp_2 ...)

   (side-condition (or (term (wv? tid θ))
                       (term (wk? tid θ))))
   
   (where (field_2 ...) (cleanWeakTable σ θ s tid (field_1 ...)))
   (where (osp_2 ...) (cleanWeakTablesAux σ θ s (osp_1 ...)))]

  ; ; {(¬ wv? ∧ ¬ wk?) ∨ closure}
  [(cleanWeakTablesAux σ θ s ((objid any) osp_1 ...))
   ((objid any) osp_2 ...)
   
   (where (osp_2 ...) (cleanWeakTablesAux σ θ s (osp_1 ...)))]
  )

; Changes to semantics of finalization
; criterion for finalization, including semantics of weak tables
(define-metafunction ext-lang
  finWeak : tid s σ θ -> any

  [(finWeak tid s σ θ)
   #t

   ; to avoid escaping to racket
   (where #f (reachCte (tid s σ θ s)))

   (where #t (marked tid θ))]

  ; default
  [(finWeak _ _ _ _)
   #f]
  )

(provide finWeak)

; predicate used to ensure that a table is not finalized if it appears as a weak
; value of a weak table: such values are removed from weak tables before running
; their finalizers
(define-metafunction ext-lang
  notFinVal : tid θ -> any

  ; ∃ tid_2/ tid_1 appears as a weak value in tid_2
  [(notFinVal tid_1 (side-condition
                     (osp_1 ...
                      (tid_2 ((\{ efield_1 ... (\[ v_1 \] = tid_1)
                                  efield_2 ... \})
                              v_2 pos))
                      osp_2 ...)
                     (term (wv? tid_2 (osp_1 ...
                                           (tid_2 ((\{ efield_1 ...
                                                       (\[ v_1 \] = tid_1)
                                                       efield_2 ... \})
                                                   v_2 pos))
                                           osp_2 ...)))))
   #f]

  ; default
  [(notFinVal _ _)
   #t]
  )

; to test notFinVal in another module
(provide notFinVal)


; predicate used to ensure that a resurrected weak key is not cleaned before running
; its finalizer: such keys are removed from weak tables only after running their
; finalizers
(define-metafunction ext-lang
  ; domain:
  ; tid for which we want to determine notCleanWeakKey x
  ; original σ store x
  ; original θ store x
  ; term t from which the original root set of references is computed
  notResWeakKey : (tid σ θ t) -> any

  ; ∃ tid_2 / tid_2 is marked for finalization and tid_1 is reachable from tid_2
  [(notResWeakKey (side-condition
                     (tid_1
                      σ
                      (osp_1 ...
                      ; tid_2 has a number in pos: it is marked for finalization
                      (tid_2 (evaluatedtable v number))
                      osp_2 ...)
                      t)
                     ; this also contemplates the case were tid_1 = tid_2
                     (term (reachCte (tid_1
                                      tid_2
                                      σ
                                      (osp_1 ...
                                       (tid_2 (evaluatedtable v number))
                                       osp_2 ...) t)))))
   #f]

  ; default
  [(notResWeakKey _)
   #t]
  )

; to test notResWeakKey in another module
(provide notResWeakKey)

; redefinition of finalization, now including semantics of weak tables

; stop-the-world gc algorithm that cleans the given θ store and returns the
; new θ store and the next object to be finalized, if any; garbage is defined
; in terms of reach and reachCte
(define-metafunction ext-lang
  ; domain:
  ; original σ x
  ; original θ x
  ; term s from which the root set is computed
  
  ; returns:
  ; new θ store
  ; tid of next table to be finalized or nil
  cleanThetaWeak : σ θ s -> any

  [(cleanThetaWeak σ θ s)
   (cleanThetaWeakAux σ θ s θ 0 nil)]
  )

(provide cleanThetaWeak)

(define-metafunction ext-lang
  ; domain:
  ; original σ x
  ; original θ x
  ; term s from which the root set is computed x
  ; instance of θ upon which recursion is done x
  ; highest priority for finalization found so far x
  ; tid of table marked for finalization or nil
  
  ; returns:
  ; new θ store
  ; tid of next table to be finalized or nil
  cleanThetaWeakAux : σ θ s θ Number v -> (θ v)
  
  ; base case
  [(cleanThetaWeakAux σ θ s () Number v)
   (() v)]

  ; objid is reachable
  [(cleanThetaWeakAux σ θ s ((objid any_1) osp_1 ... )
                      Number v_1)
   (((objid any_1) osp_2 ...) v_2)

   (side-condition (term (reachCte (objid s σ θ s))))

   ; clean the tail of the store
   (where ((osp_2 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number v_1))]

  ; {objid is not reachable}

  ; check if objid is a table, marked for finalization, with the highest
  ; priority found so far and if it does not appear as a weak value
  [(cleanThetaWeakAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...) Number_2 v_1)
   (((tid (any_1 any_2 Number_1)) osp_2 ...) v_2)

   ; tid is set for finalization, check if its priority is the highest found so
   ; far
   (side-condition (> (term Number_1)
                      (term Number_2)))

   (side-condition (term (notFinVal tid θ)))
   
   ; {tid does not appear as a value from a weak table} => it can be finalized

   ; clean the tail of the store; take tid into account for finalization
   (where ((osp_2 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number_1 tid))]

  ; {(objid is a non-rechable table ∧
  ;   (objid is not set for finalization ∨
  ;    it does not have the highest priority found so far ∨
  ;    tid appears as a weak value of another table (and, hence, cannot be finalized))
  ;
  ;   ∨
  ;
  ;   it's a non-reachable closure}
  
  ; check if objid is a table, marked for finalization, with the highest
  ; priority found so far
  [(cleanThetaWeakAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...) Number_2 v_1)
   (((tid (any_1 any_2 Number_1)) osp_2 ...) v_2)

   ; {tid is set for finalization  =>
   ; it does not have the highest priority found so far ∨
   ; tid appears as a weak value from a weak table}: should not be taken into
   ; account for finalization, nor cleaned
   
   ; clean the tail of the store
   (where ((osp_2 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number_2 v_1))
   ]

  ; {(objid is a non-rechable table ∧ objid is not set for finalization)
  ;
  ;   ∨
  ;
  ;   it's a non-reachable closure}

  ; check if it is reachable from a table marked for finalization
  [(cleanThetaWeakAux σ θ s ((objid any_1) osp_1 ...) Number v_1)
   (((objid any_1) osp_2 ...) v_2)

   ; if it is reachable from a table marked for finalization, it cannot be
   ; removed
   (side-condition (not (term (notReachFin (objid σ θ)))))

   ; {objid is reachable from a table (or its metatable) marked for
   ; finalization}
   (where ((osp_2 ...) v_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number v_1))]

  ; {((objid is a non-rechable table ∧ objid is not set for finalization)
  ;   ∨
  ;   it's a non-reachable closure)
  ;   ∧
  ;   it is not reachable from a table marked for finalization}
  
  ; osp_1 can be removed
  [(cleanThetaWeakAux σ θ s (osp_1 osp_2 ... ) Number v)
   (cleanThetaWeakAux σ θ s (osp_2 ... ) Number v)]
  )


;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                  ;;;     ;                                                  ;       
;                                 ;        ;                                                  ;       
;                                 ;                                                           ;       
;     ;;; ;    ;;;              ;;;;;;   ;;;     ; ;;;;           ;       ;   ;;;      ;;;;   ;    ;  
;    ;   ;;   ;   ;               ;        ;     ;;   ;;          ;       ;  ;   ;    ;    ;  ;  ;;   
;   ;     ;  ;                    ;        ;     ;     ;           ;  ;  ;  ;     ;        ;  ; ;     
;   ;     ;  ;                    ;        ;     ;     ;           ;  ;  ;  ;     ;   ;;;;;;  ;;;     
;   ;     ;  ;                    ;        ;     ;     ;           ; ; ; ;  ;;;;;;;  ;;    ;  ;  ;    
;   ;     ;  ;                    ;        ;     ;     ;           ; ; ; ;  ;        ;     ;  ;   ;   
;    ;   ;;   ;   ;               ;        ;     ;     ;            ;   ;    ;    ;  ;    ;;  ;    ;  
;     ;;; ;    ;;;                ;     ;;;;;;;  ;     ;            ;   ;     ;;;;    ;;;; ;  ;     ; 
;         ;                                                                                           
;    ;   ;;                                                                                           
;     ;;;;                                                                                            
;                    ;;;;;;;;;                           ;;;;;;;;;                                    


(define-metafunction ext-lang
  gcFinWeakAware : s σ θ -> (σ θ e)

  ; no finalization
  [(gcFinWeakAware s σ_1 θ_1)
   (σ_2 θ_2 nil)

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 nil) (cleanThetaWeak σ_1 θ_1 s))
   ]

  ; finalization: setting the table's pos to ⊘ should be done after cleaning
  ; weak tables
  [(gcFinWeakAware s σ_1 θ_1)
   (σ_2 θ_2 (cid (tid_1)))

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 tid_1) (cleanThetaWeak σ_1 θ_1 s))

   ; obtain metatable
   (where (osp_1 ... (tid_1 (evaluatedtable tid_2 _)) osp_2 ...) θ_2)

   ; extract finalizer
   (where (_ ... (tid_2 ((\{ _ ...
                             (\[ "__gc" \] = cid)
                             _ ... \}) _ _)) _ ...) θ_2)
   ]

  ; table set for finalization, no finalizer provided
  [(gcFinWeakAware s σ_1 θ_1)
   (σ_2 θ_2 nil)

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 tid_1) (cleanThetaWeak σ_1 θ_1 s))
   ]
  )

(provide gcFinWeakAware)


(define-metafunction ext-lang
  gcFinWeak : s σ θ -> (σ θ e)

  [(gcFinWeak s σ_1 θ_1)
   (σ_2
    (osp_3 ...
     (tid (evaluatedtable_2 any ⊘))
     osp_4 ...)
    (cid (tid)))

   (where (σ_2 (osp_1 ... (tid (evaluatedtable_1 any pos)) osp_2 ...)
               (cid (tid))) (gcFinWeakAware s σ_1 θ_1))

   ; clean weak tables
   (where (osp_3 ...
           (tid (evaluatedtable_2 any pos))
           osp_4 ...)
          (cleanWeakTables σ_2 (osp_1 ... (tid (evaluatedtable_1 any pos))
                                      osp_2 ...) s))
   ]

  [(gcFinWeak s σ_1 θ_1)
   (σ_2 θ_3 nil)

   (where (σ_2 θ_2 nil) (gcFinWeakAware s σ_1 θ_1))

   ; clean weak tables
   (where θ_3 (cleanWeakTables σ_2 θ_2 s))
   ]
  )

(provide gcFinWeak)