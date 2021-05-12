#lang racket

; GC mechanism's utilities

(require redex
         "../grammar.rkt"
         "./grammarMetaFunctions.rkt")

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
(define-metafunction ext-lang
  reach : l t σ θ -> any

  ; location belongs to the root set
  [(reach l t σ θ)
   #t

   (where (in-hole C l) t)]

  ; {l ∉ t}
  ; path through a r ∈ dom(σ) 
  [(reach l t σ θ)
   #t

   (where (side-condition (l (in-hole C r) (vsp_1 ... (r v) vsp_2 ...) θ)
                          (term (reach l v (vsp_1 ... vsp_2 ...) θ)))

          (l t σ θ))]

  ; path through a tid ∈ dom(θ) 
  [(reach l t σ θ)
   #t

   (where (side-condition (l (in-hole C tid) σ
                             (osp_1 ... (tid (evaluatedtable any _)) osp_2 ...))
                          (or (term (reach l evaluatedtable σ
                                           (osp_1 ... osp_2 ...)))
                              (term (reach l any σ
                                           (osp_1 ... osp_2 ...)))))
          (l t σ θ))]

  ; path through a cid ∈ dom(θ) 
  [(reach l t σ θ)
   #t

   (where (side-condition (l (in-hole C cid) σ
                             (osp_1 ... (cid functiondef) osp_2 ...))
                          
                          (term (reach l functiondef σ
                                       (osp_1 ... osp_2 ...))))

          (l t σ θ))]

  [(reach _ _ _ _)
   #f]
  )

(provide reach)

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
; PARAMS:
; - id of object for which we want to determine reachability
; - field (v cte) of the ephemeron table from which we want to determine
;   reachability
; - tid of the ephemeron
; - portion of the σ store not visited
; - portion of the θ store not visited
;  
; - σ θ t : original configuration
;
; PRE : {(v cte) ∈ π_1(θ(tid))}
(define-metafunction ext-lang
 
  eph : objid (v cte) tid σ θ σ θ t -> any

  ; key and value are ctes
  [(eph objid (v cte) tid
        ; remaining portions of the stores
        σ_1 (osp_3 ...
             (tid ((\{ efield_3 ...
                       (\[ v \] = cte)
                       efield_4 ... \}) any_2
                                        any_3))
             osp_4 ...)
        ; original conf
        σ_2 (osp_1 ...
             (tid ((\{ efield_1 ...
                       (\[ v \] = cte)
                       efield_2 ... \}) any_2
                                        any_3))
             osp_2 ...)
        t)
   #t
   
   (side-condition (and
                    ; we ask for reachablity of the key, from the original root
                    ; set of references and stores, discarding just a possible
                    ; reference that comes from the value of the field
                    (or (not (is_cte? (term v)))
                        (term (reachCte v t σ_2
                                        (osp_1 ...
                                         (tid ((\{ efield_3 ...
                                                   efield_4 ... \})
                                               any_2
                                               any_3))
                                         osp_2 ...))))
                    ; we ask for reachability of objid from the value; we can
                    ; discard the field and continue with the remaining portions
                    ; of σ and θ
                    (term (reachCteRec objid cte
                                       ; remaining portions of the stores
                                       σ_1 (osp_3 ...
                                            osp_4 ...)
                                       ; original conf
                                       σ_2
                                       (osp_1 ...
                                        (tid ((\{ efield_1 ...
                                                  efield_2 ... \}) any_2
                                                                   any_3))
                                        osp_2 ...)
                                       t))))
   ]

  ; {k is cte and ¬ (reachCte k t σ ...)], or ¬ (reachCte objid v ...)}
  [(eph _ _ _ _ _ _ _ _)
   #f]
  )

; auxiliary predicate of reachTable, figure 11
; performs iteration over the elements from SO (tid)
; PARAMS:
(define-metafunction ext-lang
  
  reachTableAux : objid tid σ θ σ θ t (any ...) -> any

  ; tid is an ephemerom
  [(reachTableAux objid tid σ_1 θ_1 σ_2 θ_2 t ((e_1 e_2) ...))
   #t
                  
   (where
    (side-condition (objid tid σ_1 θ_1 σ_2 θ_2 t
                           ((e_3 e_4) ... (e_5 e_6) (e_7 e_8) ...))
                    ; cannot delete table tid: we still need to
                    ; look for its keys, in case the reachability
                    ; of some key comes from another value of the
                    ; same table
                    ; (eph invokes reachCte over decreasing stores)
                    (term (eph objid (e_5 e_6) tid σ_1 θ_1 σ_2 θ_2 t)))
     
    (objid tid σ_1 θ_1 σ_2 θ_2 t ((e_1 e_2) ...)))]

  ; we can delete the binding of tid
  [(reachTableAux objid tid σ_1 θ_1 σ_2 θ_2 t (e_1 ...))
   #t
    
   (where
    (side-condition (objid tid σ_1 (osp_1 ... (tid _) osp_2 ...)
                           σ_2 θ_2 t (e_2 ... e_3 e_4 ...))
                     
                    (term (reachCteRec objid e_3 σ_1
                                       (osp_1 ... osp_2 ...)
                                       σ_2 θ_2 t)))

    (objid tid σ_1 θ_1 σ_2 θ_2 t (e_1 ...)))]

  ; default case
  [(reachTableAux _ _ _ _ _ _ _ _)
   #f]
  )

; predicate reachTable, figure 11: determines if a given objid is reachable
; from a table tid, taking into account the semantics of weak tables
(define-metafunction ext-lang
  reachTable : objid tid σ θ σ θ t -> any

  ; reachable from SO of tid
  [(reachTable objid tid σ_1 θ_1
               ; original conf
               σ_2 θ_2 t)
   #t

   (where #t (reachTableAux objid tid σ_1 θ_1 σ_2 θ_2 t (SO tid θ_1)))]

  ; reachable from metatable of tid
  [(reachTable objid tid σ_1 (osp_1 ... (tid (_ any_1 _)) osp_2 ...)
               σ_2 θ t)
   #t

   ; we need to remove tid from theta
   (where #t (reachCteRec objid any_1 σ_1 (osp_1 ... osp_2 ...) σ_2 θ t))]

  ; default case
  [(reachTable _ _ _ _ _ _ _)
   #f]
  )

(provide reachTable)

; predicate reachCte, figure 11: determines if a given cte is reachable, from
; a given term that defines the root set, and taking into account the semantics
; of weak tables
; PARAMS:
; id of cte
; term t from which the actual set of refs is computed
; portion of the σ store not visited
; portion of the θ store not visited
(define-metafunction ext-lang
  
  reachCte : objid t σ θ -> any

  [(reachCte objid t σ θ_1)
   (reachCteRec objid t σ θ_1 σ θ_1 t)

   ; for performance purposes we convert to an arbitrary non-cte value every
   ; reachCte weak key, and delete every field with a non-reactCte weak key; in
   ; this way, we guarantee that reachCte is checked once, working as a kind of
   ; memoization
   (where θ_2 (cleanReachCteWeakKeys σ θ_1 t))
   ])

(provide reachCte)

; traverses a given θ store, converting its reachCte weak keys into arbitrarty
; non-cte values
(define-metafunction ext-lang
  cleanReachCteWeakKeys : σ θ t -> θ

  [(cleanReachCteWeakKeys σ θ t)
   (cleanReachCteWeakKeysAux θ σ θ t)])

; function aux. to cleanReachCteWeakKeys that actually performs the recursion
; over θ
; PARAMS:
; θ_1 : the instance of θ over which recursion is done
; σ θ_2 t : the original configuration
;
; RETURNS:
; the modified θ store
(define-metafunction ext-lang
  cleanReachCteWeakKeysAux : θ σ θ t -> θ

  [(cleanReachCteWeakKeysAux () σ θ t)
   ()]

  [(cleanReachCteWeakKeysAux ((cid functiondef) osp_1 ...) σ θ t)
   ((cid functiondef) osp_2 ...)

   (where (osp_2 ...) (cleanReachCteWeakKeysAux (osp_1 ...) σ θ t))]

  [(cleanReachCteWeakKeysAux ((tid ((\{ efield_1 ... \}) v pos)) osp_1 ...) σ θ t)
   ((tid ((\{ efield_2 ... \}) v pos)) osp_2 ...)

   (where #t (wk? tid θ))

   (where (efield_2 ...) (replaceWeakKeys (efield_1 ...) tid σ θ t))

   (where (osp_2 ...) (cleanReachCteWeakKeysAux (osp_1 ...) σ θ t))]

  [(cleanReachCteWeakKeysAux ((tid any) osp_1 ...) σ θ t)
   ((tid any) osp_2 ...)

   (where (osp_2 ...) (cleanReachCteWeakKeysAux (osp_1 ...) σ θ t))])

; receives the fields and tid of a table with weak keys, transform into
; numbers the reactCte weak keys
(define-metafunction ext-lang
  replaceWeakKeys : (efield ...) tid σ θ t -> (efield ...)

  [(replaceWeakKeys ((\[ v_1 \] = v_2) ...) tid σ θ t)
   (replaceWeakKeysAux ((\[ v_1 \] = v_2) ...) tid Number σ θ t)

   (where Number ,(add1 (argmax max (filter
                                     (redex-match? ext-lang Number)
                                     (term (v_1 ... 1))))))]
  )

; function aux. to replaceWeakKeys that actually performs the recursion
; PARAMS:
; efield ... : the fields of the table
; tid : id of the table
; Number : the number to be used for the next reachCte weak key to be replaced
; σ θ t : the original configuration
(define-metafunction ext-lang
  replaceWeakKeysAux : (efield ...) tid Number σ θ t -> (efield ...)

  [(replaceWeakKeysAux () tid Number σ θ t)
   ()]

  [(replaceWeakKeysAux ((\[ cte \] = v_1) efield_1 ...) tid Number σ θ t)
   ((\[ Number \] = v_1) efield_4 ...)
   
   (where (osp_1 ... (tid ((\{ efield_2 ... (\[ cte \] = v_1)
                               efield_3 ... \}) v_2 pos))
           osp_2 ...) θ)

   ; check reachCte without considering the field (\[ cte \] = v_1)
   (where #t (reachCteRec cte t σ
                          (osp_1 ...
                           (tid ((\{ efield_2 ...
                                     efield_3 ... \}) v_2 pos))
                           osp_2 ...)
                          σ (osp_1 ... (tid ((\{ efield_2 ...
                                                 efield_3 ... \}) v_2 pos ))
                             osp_2 ...) t))

   (where (efield_4 ...) (replaceWeakKeysAux (efield_1 ...) tid
                                             ,(add1 (term Number)) σ θ t))
   ]

  ; {cte is non reachCte}
  [(replaceWeakKeysAux ((\[ cte \] = v) efield_1 ...) tid Number σ θ t)
   (efield_2 ...)

   (where (efield_2 ...) (replaceWeakKeysAux (efield_1 ...) tid Number σ θ t))]

  [(replaceWeakKeysAux ((\[ v_1 \] = v_2) efield_1 ...) tid Number σ θ t)
   ((\[ v_1 \] = v_2) efield_2 ...)

   (where (efield_2 ...) (replaceWeakKeysAux (efield_1 ...) tid Number σ θ t))]
  )

; to make reachCte continue an iteration over the remaining portion of the
; stores
; PARAMS:
; id of cte
; term t from which the actual set of refs is computed
; portion of the σ store not visited
; portion of the θ store not visited
  
; σ θ t : original configuration
(define-metafunction ext-lang
  
  reachCteRec : objid t σ θ σ θ t -> any

  ; objid ∈ root set
  [(reachCteRec objid (in-hole C objid) _ _ _ _ _)
   #t]

  ; path through a r ∈ dom(σ)
  [(reachCteRec objid t_1 σ_1 θ_1 σ_2 θ_2 t_2)
   #t

   (where (side-condition
           (objid (in-hole C r) (vsp_1 ... (r v) vsp_2 ...) θ_1 σ_2 θ_2 t_2)
           (term (reachCteRec objid v (vsp_1 ... vsp_2 ...) θ_1
                              σ_2 θ_2 t_2)))

          (objid t_1 σ_1 θ_1 σ_2 θ_2 t_2))]

  ; path through a tid ∈ dom(θ)
  [(reachCteRec objid t_1 σ_1 θ_1 σ_2 θ_2 t_2)
   #t

   (where (side-condition (objid (in-hole C tid) σ_1 θ_1 σ_2 θ_2 t_2)
                          (term (reachTable objid tid σ_1 θ_1 σ_2 θ_2 t_2)))

          (objid t_1 σ_1 θ_1 σ_2 θ_2 t_2))]

  ; path through a cid ∈ dom(θ)
  [(reachCteRec objid t_1 σ_1 θ_1 σ_2 θ_2 t_2)
   #t

   (where (side-condition
           (objid (in-hole C cid) σ_1 (osp_1 ... (cid functiondef) osp_2 ...)
                  σ_2 θ_2 t_2)
           (term (reachCteRec objid functiondef σ_1
                              (osp_1 ... osp_2 ...)
                              σ_2 θ_2 t_2)))
          
          (objid t_1 σ_1 θ_1 σ_2 θ_2 t_2))]

  ; default
  [(reachCteRec _ _ _ _ _ _ _)
   #f]
  )

(provide reachCteRec)
; implements the removal of fields of a given weak table, as specified in
; figure 12, condition "wt"
;
; PARAMS:
; - σ θ t : original configuration
; - tid of the table being cleaned
; - fields of the table being cleaned

; PRE : {(field ...) belongs to a weak table (condition "weakness")}
(define-metafunction ext-lang
  cleanWeakTable : σ θ s tid (field ...) -> (field ...)

  [(cleanWeakTable σ θ s tid ())
   ()]

  ; field not cleaned
  [(cleanWeakTable σ θ s tid ((\[ v_1 \] = v_2) field_1 ...))
   ((\[ v_1 \] = v_2) field_2 ...)

   (where (osp_1 ... (tid ((\{ efield_1 ... (\[ v_1 \] = v_2) efield_2 ... \})
                           v_3 pos)) osp_2 ...)
          θ)
   
   ; condition "reach"
   (where #t ,(and (or (not (term (wk? tid θ)))
                       (not (is_cte? (term v_1)))
                       ; the ref to the key cannot come from the value of
                       ; the field
                       (term (reachCte v_1 s σ (osp_1 ...
                                                (tid ((\{ efield_1 ...
                                                          efield_2 ... \})
                                                      v_3 pos))
                                                osp_2 ...))))
                   (or (not (term (wv? tid θ)))
                       (not (is_cte? (term v_2)))
                       ; we must let the field (\[ v_1 \] = v_2) in θ,
                       ; since the refs to v_2 could be in v_1; still,
                       ; when deciding reachability of v_2, there is no
                       ; risk of considering v_2 itself, since it is a
                       ; weak value
                       (term (reachCte v_2 s σ θ)))))

   (where (field_2 ...) (cleanWeakTable σ θ s tid (field_1 ...)))]

  ; {key or value weak, cte and not reachable}
  ; field could be removed, but check fin_key first
  [(cleanWeakTable σ θ s tid_1 ((\[ tid_2 \] = v) field_1 ...))
   ((\[ tid_2 \] = v) field_2 ...)

   ; condition fin_key: if tid_2 is marked for finalization, or reachable
   ; from a table marked for finalization (i.e. it will be resurrected, for
   ; finalization purposes), then, it should not be removed from weak keys;
   ; we check if this is the case with the predicate notReachFin
   (where #t ,(and (term (wk? tid_1 θ))
                   (not (term (notReachFin tid_2 σ θ)))))

   (where (field_2 ...) (cleanWeakTable σ θ s tid_1 (field_1 ...)))]

  ; {key or value weak, cte, not reachable and not fin_key}
  ; field can be removed
  [(cleanWeakTable σ θ s tid ((\[ v_1 \] = v_2) field_1 ...))
   (field_2 ...)
   
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

; checks if a given reference is not reachable (without considering reachability
; from weak tables) from tables marked for finalization
(define-metafunction ext-lang
  
  notReachFin : l σ θ -> any

  ; reachable from a table marked 
  [(notReachFin l σ θ)
   #f

   (where
    (side-condition
     ; for performance, we check here if tid is marked
     (l σ (osp_1 ... (tid (evaluatedtable e number)) osp_2 ...))
     ; l could be equal to tid, in which case we found that l is
     ; marked for finalization => it must not be removed
     (term (reach l tid σ
                  (osp_1 ... (tid (evaluatedtable e number))
                         osp_2 ...))))

    (l σ θ))]

  ; default case
  [(notReachFin _ _ _)
   #t]
  )

(provide notReachFin)