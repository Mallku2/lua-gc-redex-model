#lang racket

; GC mechanism's utilities

(require redex
         "../grammar.rkt"
         "./objStoreMetafunctions.rkt"
         "./valStoreMetafunctions.rkt"
         "./tablesMetafunctions.rkt"
         ;"./delta.rkt"
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
; Codifies the logic behind marking an object for finalization
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
  [(maxPosAux (osp ... (cid _)) max)
   (maxPosAux (osp ...) max)]
  )

(define-metafunction ext-lang
  marked : tid θ -> any

  [(marked tid (_ ... (tid (_ _ number)) _ ...))
   #t]

  ; default case
  [(marked _ _)
   #f]
  )

(provide marked)

; Checks if a given location is reachable from table marked for finalization
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

; compares a given tid finalization pos, with any other table marked for
; finalization (regardless the value of fin(tid))
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

  ; Hack to maintain stdout in σ
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

; Stop-the-world gc algorithm that cleans the given θ store and returns the
; next object to be finalized, if any. Garbage is defined in terms of reach

(define-metafunction ext-lang
  cleanTheta : σ θ s -> any

  [(cleanTheta σ θ s)
   (cleanThetaAux σ θ s θ 0 0)]
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

; members of tid referenced by strong references
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

(provide cteKeys)

(define-metafunction ext-lang
  cteValues : (field ...) -> (e ...)

  [(cteValues ())
   ()]
  
  [(cteValues ((\[ _ \] = cte) field ...))
   ,(append (term (cte))
            (term (cteValues (field ...))))]

  [(cteValues (_ field ...))
   (cteValues (field ...))]
  )

(provide cteValues)

; extracts 
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

  ; Default case: no CTEs in field
  [(cteFields (_ field ...))
   (cteFields (field ...))]
  )

(define-metafunction ext-lang
  cteEphemeron : (field ...) -> ((e e) ...)

  [(cteEphemeron ())
   ()]
  
  [(cteEphemeron ((\[ v \] = cte) field ...))
   ,(append (term ((v cte)))
            (term (cteEphemeron (field ...))))]

  ; Default case: no CTEs in field
  [(cteEphemeron (_ field ...))
   (cteEphemeron (field ...))]
  )

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

(provide cteEphemeron)

; performs iteration over the elements from SO (tid)
(define-metafunction ext-lang
  reachTableAux : (objid tid σ θ any (any ...)) -> any

  ; Cannot delete table tid: we still need to look for its keys, in case
  ; the reachability of some key comes from another value of the same table
  [(reachTableAux (side-condition (objid tid σ θ any 
                                         (any_1 ... (e_1 e_2) any_2 ...))
                                  (term (eph objid (e_1 e_2) tid any σ θ))))
   #t
   ]

  ; We can delete the binding of tid
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

(define-metafunction ext-lang
  reachCte : (objid (side-condition any (is_term? (term any))) σ θ
                    (side-condition any (is_term? (term any)))) -> any

  ; objid ∈ any
  [(reachCte (objid (in-hole C objid) _ _ _))
   #t
   ]

  ; path through a r ∈ dom(σ)
  [(reachCte (side-condition
              (objid (in-hole C r) (vsp_1 ... (r v) vsp_2 ...) θ any_2)
              (term (reachCte (objid v (vsp_1 ... vsp_2 ...) θ any_2)))))
   #t
   ]

  ; path through a tid ∈ dom(θ)
  [(reachCte (side-condition
              (objid (in-hole C tid) σ (osp_1 ... (tid any_2) osp_2 ...) any_3)
              (term (reachTable objid tid σ
                                (osp_1 ... (tid any_2) osp_2 ...)
                                any_3))))
   #t
   ]

  ; path through a cid ∈ dom(θ)
  [(reachCte (side-condition
              (objid (in-hole C cid) σ (osp_1 ... (cid functiondef) osp_2 ...)
                     any_3)
              (term (reachCte (objid functiondef σ
                                     (osp_1 ... osp_2 ...)
                                     any_3)))))
   #t
   ]

  ; default
  [(reachCte _)
   #f
   ]
  )

(provide reachCte)

; PRE : {(field ...) belongs to a weak table (condition "weakness")}
(define-metafunction ext-lang
  cleanWeakTable : σ θ s tid (field ...) -> (field ...)

  [(cleanWeakTable σ θ s tid ())
   ()]

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

  ; {key or value not reachable}
  [(cleanWeakTable σ θ s tid ((\[ v_1 \] = v_2) field_1 ...))
   ; {key or value not reachable}
   (field_2 ...)

   ; fin key
   (side-condition (or (not (and (is_tid? (term v_1))
                                 (term (wk? tid θ))))
                       (not (term (marked v_1 θ)))))
   
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

; predicate that ensures that a table is not finalized if it appears as value
; of a weak table (such values are removed from weak tables before running their
; finalizers)
(define-metafunction ext-lang
  notFinVal : tid θ -> any

  ; ∃ tid_2/ tid_2 is a weak table ∧ tid_1 appears as value of tid_2
  [(notFinVal tid_1 (side-condition
                     (osp_1 ...
                      (tid_2 ((\{ efield_1 ... (\[ v_1 \] = tid_1)
                                  efield_2 ... \})
                              v_2 pos))
                      osp_2 ...)
                     (or (term (wk? tid_2 (osp_1 ...
                                           (tid_2 ((\{ efield_1 ...
                                                       (\[ v_1 \] = tid_1)
                                                       efield_2 ... \})
                                                   v_2 pos))
                                           osp_2 ...)))
                         (term (wv? tid_2 (osp_1 ...
                                           (tid_2 ((\{ efield_1 ...
                                                       (\[ v_1 \] = tid_1)
                                                       efield_2 ... \})
                                                   v_2 pos))
                      osp_2 ...))))))
   #f
   ]

  ; default
  [(notFinVal _ _)
   #t
   ]
  )
(provide notFinVal)

; Redefinition of finalization, now including semantics of weak tables
; Stop-the-world gc algorithm that cleans the given θ store and returns the
; next object to be finalized, if any. Garbage is defined in terms of reach
; and reachCte

(define-metafunction ext-lang
  cleanThetaWeak : σ θ s -> any

  [(cleanThetaWeak σ θ s)
   (cleanThetaWeakAux σ θ s θ 0 0)]
  )

(provide cleanThetaWeak)

(define-metafunction ext-lang
  ; σ x θ x s x θ x highest priority found x tid of table marked for fin 
  cleanThetaWeakAux : σ θ s θ Number v -> (θ v)
  
  ; base case
  [(cleanThetaWeakAux σ θ s () Number v)
   (() v)]

  ; objid is reachable
  [(cleanThetaWeakAux σ θ s ((objid any_1) osp_1 ... )
                      Number v)
   (((objid any_1) osp_2 ...) any_2)

   (side-condition (term (reachCte (objid s σ θ s))))

   ; clean the tail of the store
   (where ((osp_2 ...) any_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number v))]

  ; {objid is not reachable}
  ; check if it is a table, marked for finalization
  [(cleanThetaWeakAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...)
                    Number_2 v)
   
   (((tid (any_1 any_2 Number_1)) osp_2 ...) any_3)
   ; finalizer set, is not the highest priority found so far
   (side-condition (<= (term Number_1)
                       (term Number_2)))

   ; clean the tail of the store
   (where ((osp_2 ...) any_3) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number_2
                                                 v))]

  ; {(it is non reachable table tid => tid is not set for finalization ∨ has the
  ; highest priority found so far) ∨ it's a non-reachable closure}
  [(cleanThetaWeakAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...) Number_2 v)
   (((tid (any_1 any_2 Number_1)) osp_2 ...) any_3)

   ; tid is set for finalization: must be the next one
   (side-condition (> (term Number_1)
                      (term Number_2)))

   (side-condition (term (notFinVal tid θ)))
   ; {tid does not appear as a value from a weak table}
   
   ; clean the tail of the store
   (where ((osp_2 ...) any_3) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number_1
                                                 tid))
   ]

  ; {(it is a non reachable table tid => (tid is not set for finalization ∨ has
  ; the highest priority found so far ∨ appears as a value of a weak table)) ∨
  ; it's a non-reachable closure}
    [(cleanThetaWeakAux σ θ s ((tid (any_1 any_2 Number_1)) osp_1 ...) Number_2 v)
   (((tid (any_1 any_2 Number_1)) osp_2 ...) any_3)

   (side-condition (> (term Number_1)
                      (term Number_2)))
   ; {tid is set for finalization and has the highest priority =>
   ; tid appears as a value from a weak table}: should not be taken into
   ; account for finalization, nor cleaned
   
   ; clean the tail of the store
   (where ((osp_2 ...) any_3) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number_2 v))
   ]

  ; {(it is a non reachable table tid => tid is not set for finalization ∨
  ;  it's a non-reachable closure}
  [(cleanThetaWeakAux σ θ s ((objid any_1) osp_1 ...) Number v)
   (((objid any_1) osp_2 ...) any_2)

   ; if it is reachable from a table marked for finalization, it cannot be
   ; removed
   (side-condition (not (term (notReachFin (objid σ θ)))))

   ; {objid is reachable from a table (or its metatable) marked for
   ; finalization}
   (where ((osp_2 ...) any_2) (cleanThetaWeakAux σ θ s (osp_1 ... ) Number v))]

  ; {(it is a non reachable table tid => tid is not set for finalization ∨
  ;  it's a non-reachable closure) ∧ is not reachable from a table marked for
  ; finalization} : it can be removed
  [(cleanThetaWeakAux σ θ s (osp_1 osp_2 ... ) Number v)
   (cleanThetaWeakAux σ θ s (osp_2 ... ) Number v)]
  )

(define-metafunction ext-lang
  gcFinWeakAware : s σ θ -> (σ θ e)

  ; no finalization
  [(gcFinWeakAware s σ_1 θ_1)
   (σ_2 θ_2 nil)

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 0) (cleanThetaWeak σ_1 θ_1 s))
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