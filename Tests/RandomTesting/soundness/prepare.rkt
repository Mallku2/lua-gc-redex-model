#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Meta-functions/grammarMetaFunctions.rkt"
         "../../../Meta-functions/substitution.rkt"
         "../../../Meta-functions/objStoreMetaFunctions.rkt"
         )


;                                               
;                                               
;                                               
;               ;                               
;               ;                               
;                                               
;    ;;;;;    ;;;       ;;; ;  ;;;;;;     ;;;;  
;   ;     ;     ;      ;   ;;  ;  ;  ;   ;    ; 
;   ;           ;     ;     ;  ;  ;  ;        ; 
;   ;;;;        ;     ;     ;  ;  ;  ;   ;;;;;; 
;       ;;;     ;     ;     ;  ;  ;  ;  ;;    ; 
;         ;     ;     ;     ;  ;  ;  ;  ;     ; 
;   ;     ;     ;      ;   ;;  ;  ;  ;  ;    ;; 
;    ;;;;;   ;;;;;;;    ;;; ;  ;  ;  ;   ;;;; ; 
;                           ;                   
;                      ;   ;;                   
;                       ;;;;                    
;                                               

; TODO: abstract patterns
; extracts val. references from a term t
(define (get_val_refs t)
  (map (lambda (match)
         ; extract bindings from the match; filter bindings for symbol 'r;
         ; extract the associated expression
         (bind-exp (list-ref (filter (lambda (b)
                                       (equal? (bind-name b) 'r))
                                     (match-bindings match)) 0)))
       (let ([match (redex-match ext-lang
                                 (in-hole C r)
                                 (term ,t))])
         (if match
             match
             '()))))

; free val refs in t
(define-metafunction ext-lang
  free_val_refs : (vsp ...) t -> (r ...)

  ; discard refStdout
  [(free_val_refs ((refStdout String) (r v) ...) t)
   (free_val_refs ((r v) ...) t)]
  
  [(free_val_refs ((r_1 v) ...) t)
   (r_3 ...)

   (where (r_2 ...) ,(get_val_refs (term t)))
   (where (r_3 ...) ,(remove-duplicates
                      (remove* (term (r_1 ...)) (term (r_2 ...)))))]
  )

(provide free_val_refs)

; free val refs in θ (from the environment of closures)
(define-metafunction ext-lang
  ; it cannot be σ and θ since the domains may not be well-formed
  free_val_refs_theta : (vsp ...) (osp ...) -> (r ...)

  ; discard refStdout
  [(free_val_refs_theta (vsp ...) ())
   ()]
  
  [(free_val_refs_theta (vsp ...) ((_ functiondef) osp ...))
   (r_3 ...)

   (where (r_1 ...) (free_val_refs (vsp ...) functiondef))
   (where (r_2 ...) (free_val_refs_theta (vsp ...) (osp ...)))
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))]

  [(free_val_refs_theta (vsp ...) ((_ (tableconstructor _ _)) osp ...))
   (r_3 ...)

   (where (r_1 ...) (free_val_refs (vsp ...) tableconstructor))
   (where (r_2 ...) (free_val_refs_theta (vsp ...) (osp ...)))
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))]
  )


;                                               
;                                               
;                                               
;            ;                                  
;     ;      ;                   ;              
;     ;      ;                   ;              
;   ;;;;;;   ; ;;;;     ;;;    ;;;;;;     ;;;;  
;     ;      ;;   ;;   ;   ;     ;       ;    ; 
;     ;      ;     ;  ;     ;    ;            ; 
;     ;      ;     ;  ;     ;    ;       ;;;;;; 
;     ;      ;     ;  ;;;;;;;    ;      ;;    ; 
;     ;      ;     ;  ;          ;      ;     ; 
;     ;      ;     ;   ;    ;    ;      ;    ;; 
;      ;;;   ;     ;    ;;;;      ;;;    ;;;; ; 
;                                               
;                                               
;                                               
;                                               

; extracts tids from a term t
(define (get_tids t)
  (map (lambda (match)
         ; extract bindings from the match; filter bindings for symbol 'r;
         ; extract the associated expression
         (bind-exp (list-ref (filter (lambda (b)
                                       (equal? (bind-name b) 'tid))
                                     (match-bindings match)) 0)))
       (let ([match (redex-match ext-lang
                                 (in-hole C tid)
                                 (term ,t))])
         (if match
             match
             '()))))

; PRE : {any ∈ s ∪ e}
(define-metafunction ext-lang
  free_tids : (osp ...) t -> (tid ...)

  [(free_tids ((any object) ...) t)
   (tid_2 ...)

   (where (tid_1 ...) ,(get_tids (term t)))
   (where (tid_2 ...) ,(remove-duplicates
                        (remove* (term (any ...)) (term (tid_1 ...)))))]
  )

(provide free_tids)

; free tids in θ
(define-metafunction ext-lang
  free_tids_theta : (osp ...) -> (tid ...)

  [(free_tids_theta (osp ...))
   (free_tids_theta_aux (osp ...) (osp ...))]
  )

(define-metafunction ext-lang
  free_tids_theta_aux : (osp ...) (osp ...) -> (tid ...)

  [(free_tids_theta_aux (osp ...) ())
   ()]

  ; a functiondef shouldn't have a tid, but redex-check (and the grammar) allow
  ; them
  [(free_tids_theta_aux (osp_1 ...) ((_ functiondef) osp_2 ...))
   (tid_3 ...)

   (where (tid_1 ...) (free_tids (osp_1 ...) functiondef))
   (where (tid_2 ...) (free_tids_theta_aux (osp_1 ...) (osp_2 ...)))
   (where (tid_3 ...) ,(remove-duplicates (term (tid_1 ... tid_2 ...))))]

  ; table with meta-table set
  [(free_tids_theta_aux (osp_1 ...) ((_ (tableconstructor any _)) osp_2 ...))
   (tid_4 ...)

   (where (tid_1 ...) (free_tids (osp_1 ...) tableconstructor))
   ; check if the metatable is actually in (osp_1 ...) or not
   (where (tid_2 ...) (free_tids (osp_1 ...) any))
   (where (tid_3 ...) (free_tids_theta_aux (osp_1 ...) (osp_2 ...)))
   (where (tid_4 ...) ,(remove-duplicates (term (tid_1 ...
                                                 tid_2 ...
                                                 tid_3 ...))))]
  )

(define-metafunction ext-lang
  clean_free_objid_sigma : (vsp ...) (osp ...) -> (vsp ...)

  [(clean_free_objid_sigma () _)
   ()]

  [(clean_free_objid_sigma ((r objid) vsp_1 ...)
                           (osp_1 ... (objid any) osp_2 ...))
   ((r objid) vsp_2 ...)
   
   (where (vsp_2 ...) (clean_free_objid_sigma (vsp_1 ...)
                                              (osp_1 ... (objid any) osp_2 ...)))]

  [(clean_free_objid_sigma ((r objid) vsp_1 ...) (osp ...))
   ; {objid ∉ dom(osp ...)}
   ((r 1) vsp_2 ...)
   
   (where (vsp_2 ...) (clean_free_objid_sigma (vsp_1 ...) (osp ...)))]

  ; {vsp_1 ≠ (r, tid)}
  [(clean_free_objid_sigma (vsp_1 vsp_2 ...) (osp ...))
   (vsp_1 vsp_3 ...)

   (where (vsp_3 ...) (clean_free_objid_sigma (vsp_2 ...) (osp ...)))])


; extract closures ids from a term t
(define (get_clids t)
  (map (lambda (match)
         ; extract bindings from the match; filter bindings for symbol 'r;
         ; extract the associated expression
         (bind-exp (list-ref (filter (lambda (b)
                                       (equal? (bind-name b) 'cid))
                                     (match-bindings match)) 0)))
       (let ([match (redex-match ext-lang
                                 (in-hole C cid)
                                 (term ,t))])
         (if match
             match
             '()))))

(provide free_clids)
; closures ids not bound in θ
(define-metafunction ext-lang
  free_clids : (osp ...) t -> (cid ...)

  [(free_clids ((any object) ...) t)
   (cid_2 ...)

   (where (cid_1 ...) ,(get_clids (term t)))
   (where (cid_2 ...) ,(remove-duplicates
                        (remove* (term (any ...)) (term (cid_1 ...)))))]
  )

; free cids in θ
(define-metafunction ext-lang
  free_clids_theta : (osp ...) -> (cid ...)

  [(free_clids_theta (osp ...))
   (free_clids_theta_aux (osp ...) (osp ...))]
  )

(define-metafunction ext-lang
  free_clids_theta_aux : (osp ...) (osp ...) -> (cid ...)

  [(free_clids_theta_aux (osp ...) ())
   ()]

  ; a functiondef shouldn't have a cid, but redex-check (and the grammar) allow
  ; them
  [(free_clids_theta_aux (osp_1 ...) ((_ functiondef) osp_2 ...))
   (cid_3 ...)

   (where (cid_1 ...) (free_clids (osp_1 ...) functiondef))
   (where (cid_2 ...) (free_clids_theta_aux (osp_1 ...) (osp_2 ...)))
   (where (cid_3 ...) ,(remove-duplicates (term (cid_1 ... cid_2 ...))))]

  ; table without meta-table set
  [(free_clids_theta_aux (osp_1 ...) ((_ (tableconstructor _ _)) osp_2 ...))
   (cid_3 ...)

   (where (cid_1 ...) (free_clids (osp_1 ...) tableconstructor))
   (where (cid_2 ...) (free_clids_theta_aux (osp_1 ...) (osp_2 ...)))
   (where (cid_3 ...) ,(remove-duplicates (term (cid_1 ... cid_2 ...))))]
  )

; bound free tids, cids from a given conf; enforces well-formedness of the domain
; and img of given stores
(define-metafunction ext-lang
  ; note that the domain is (osp ...) instead of θ, to allow
  ; for ill-formed θ stores (e.g., repeated locations in the domain)
  close_fix_theta_sigma : σ (osp ...) t -> (σ θ t)

  [(close_fix_theta_sigma (vsp ...) (osp_1 ...) t_1)
   (σ_2 θ t_2)

   ; get free val. refs from t and θ
   (where (r_1 ...) (free_val_refs (vsp ...) t_1))
   (where (r_2 ...) (free_val_refs_theta (vsp ...) (osp_1 ...)))
   ; remove repeated refs.
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))
   
   ; add dummy vals. and bound free refs
   (where σ_1 (vsp ... (r_3 1) ... ))
   
   ; get free tids, cids from θ and t
   (where (tid_1 ...) (free_tids (osp_1 ...) t_1))
   (where (tid_2 ...) (free_tids_theta (osp_1 ...)))
   (where (tid_3 ...) ,(remove-duplicates (term (tid_1 ... tid_2 ...))))
   
   (where (cid_1 ...) (free_clids (osp_1 ...) t_1))
   (where (cid_2 ...) (free_clids_theta (osp_1 ...)))
   (where (cid_3 ...) ,(remove-duplicates (term (cid_1 ... cid_2 ...))))
   
   ; add dummy tables and closures to bound tids and cids
   (where (osp_2 ...) (osp_1 ...
                       (tid_3 ((\{ \}) nil ⊥)) ...
                       ; we must avoid breaking the invariant that refers to
                       ; closure caching: there should be only one instance
                       ; of a given closure value in store
                       ; we introduce arbitrary differences (e.g., through cid_3 ...)
                       (cid_3 (function x () (return cid_3) end)) ...))
   ; ensure well-formedness of dom((osp_2 ...)) and img((osp_2 ...)))
   (where (θ ((objid_1 objid_2) ...)) (fix_theta_dom_img (osp_2 ...)))
   ; apply the expected substitutions in t_1 and σ_1
   (where t_2 (subst t_1 ((objid_1 objid_2) ...)))
   ; clean free obijds in σ
   (where σ_2 (clean_free_objid_sigma σ_1 θ))]
  )

; redex-check could generate terms with refs (objr number_1) and (cl number_1)
; fix_theta_dom implements a simple fix to that
; it also enforces well-formedness of the img
; RETURNS
; modified θ together with the substitutions of objid that the user of this function
; needs to perform on the term and σ in order to coincide with the returned θ
(define-metafunction ext-lang
  fix_theta_dom_img : (osp ...) -> (θ ((objid objid) ...))

  [(fix_theta_dom_img ())
   (() ())]

  [(fix_theta_dom_img (((any natural) object) osp ...))
   (θ_2 ((objid_1 objid_2) ...))

   (where (θ_1 ((objid_1 objid_2) ...))
          (fix_theta_dom_img_aux (((any natural) object) osp ...) natural ()))
   ; apply substitution in img of θ_1
   (where θ_2 (substTheta θ_1 ((objid_1 objid_2) ...)))]
   
  )

; enforces a well-formed dom(θ) by replacing each reference with a
; reference that is unique; it also enforces well-formedness of the img
; PRE : {θ_1 ++ θ_2 = θ}
(define-metafunction ext-lang
  fix_theta_dom_img_aux : (osp ...) natural ((objid objid) ...) -> (θ ((objid objid) ...))

  [(fix_theta_dom_img_aux () natural ((objid_1 objid_2) ...))
   (() ((objid_1 objid_2) ...))]

  [(fix_theta_dom_img_aux (((objr natural_1) (tableconstructor_1 any pos))
                           osp_1 ...)
                          natural_2
                          ((objid_1 objid_2) ...))
   ((((objr natural_2)
      ; in order to bound free ids in tableconstructor_1,
      ; close_term_meta returned a functiondef; we put it into a new
      ; table
      ((\{ (\[ 1 \] = functiondef) \}) any_2 pos))
     osp_3 ...)

    ((objid_1 objid_2) ...
     ((objr natural_1) (objr natural_2))
     (objid_3 objid_4) ...))

   ; table constructor must be well formed: deleted nil, nan or repeated keys;
   ; bound free variables
   (where tableconstructor_2 (fix_tableconstructor tableconstructor_1))
   ; the following holds when tableconstructor_2 has free variables id
   (where functiondef (close_term_meta tableconstructor_2))

   ; next pos in θ
   (where natural_3 ,(+ 1 (term natural_2)))

   (where ((osp_3 ...) ((objid_3 objid_4) ...))
          (fix_theta_dom_img_aux (osp_1 ...)
                                 natural_3
                                 ()))]

  [(fix_theta_dom_img_aux (((objr natural_1) (tableconstructor_1 any pos))
                           osp_1 ...)
                          natural_2
                          ((objid_1 objid_2) ...))
   ((((objr natural_2) (tableconstructor_2 any pos)) osp_3 ...)

    ((objid_1 objid_2) ...
     ((objr natural_1) (objr natural_2))
     (objid_3 objid_4) ...))

   ; table constructor must be well formed: deleted nil, nan or repeated keys;
   ; bound free variables
   (where tableconstructor_2 (fix_tableconstructor tableconstructor_1))
   ; the following holds when tableconstructor does not have free vars
   (where tableconstructor_2 (close_term_meta tableconstructor_2))

   ; next pos in θ
   (where natural_3 ,(+ 1 (term natural_2)))

   (where ((osp_3 ...) ((objid_3 objid_4) ...))
          (fix_theta_dom_img_aux (osp_1 ...)
                                 natural_3
                                 ()))]

  
  [(fix_theta_dom_img_aux (((cl natural_1) functiondef_1)
                           osp_1 ...)
                          natural_2
                          ((objid_1 objid_2) ...))
   ((((cl natural_2) functiondef_2) osp_3 ...)

    ((objid_1 objid_2) ...
     ((cl natural_1) (cl natural_2))
     (objid_3 objid_4) ...))

   ; functiondef_1 must be well-formed
   (where functiondef_2 (close_term_meta functiondef_1))

   ; next pos in θ
   (where natural_3 ,(+ 1 (term natural_2)))

   (where ((osp_3 ...) ((objid_3 objid_4) ...))
          (fix_theta_dom_img_aux (osp_1 ...)
                                 natural_3
                                 ()))]
  )

; fix repeated keys and nil, nan keys
(define-metafunction ext-lang
  fix_tableconstructor : tableconstructor -> tableconstructor

  [(fix_tableconstructor (\{ field_1 ... \}))
   (\{ field_2 ... \})

   (where (field_2 ...) (fix_tableconstructor_aux (field_1 ...)))]
  )

(define-metafunction ext-lang
  fix_tableconstructor_aux : (field ...) -> (field ...)

  [(fix_tableconstructor_aux ())
   ()]

  ; discard fields with nil or nan key
  [(fix_tableconstructor_aux ((\[ v \] = _) field ...))
   (fix_tableconstructor_aux (field ...))

   (side-condition (or (is_nil? (term v))
                       (equal? (term v)
                               +nan.0)))]

  ; {v ≠ nil, nan}
  ; to avoid discarding nil-valued fields
  [(fix_tableconstructor_aux ((\[ v \] = nil) field_1 ...))
   ((\[ v \] = 1) field_2 ...)

   (where (field_2 ...) (fix_tableconstructor_aux (field_1 ...)))]

  ; default: field does not have a nil or nan key, or nil value
  [(fix_tableconstructor_aux ((\[ v_1 \] = v_2) field_1 ...))
   ((\[ v_1 \] = v_2) field_2 ...)

   (where (field_2 ...) (fix_tableconstructor_aux (field_1 ...)))]
  )
;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;       ;;;     ;                                                      ;;;        ;                                     
;      ;        ;                                                     ;          ;     ;                                
;      ;                                                              ;          ;     ;                                
;    ;;;;;;   ;;;     ;;   ;;             ;;;      ;;;    ; ;;;;    ;;;;;;      ;    ;;;;;;     ;;;      ; ;;;  ;;;;;;  
;      ;        ;      ;   ;             ;   ;    ;   ;   ;;   ;;     ;         ;      ;       ;   ;     ;;   ; ;  ;  ; 
;      ;        ;       ; ;             ;        ;     ;  ;     ;     ;        ;       ;      ;     ;    ;      ;  ;  ; 
;      ;        ;        ;              ;        ;     ;  ;     ;     ;        ;       ;      ;     ;    ;      ;  ;  ; 
;      ;        ;        ;              ;        ;     ;  ;     ;     ;       ;        ;      ;;;;;;;    ;      ;  ;  ; 
;      ;        ;       ; ;             ;        ;     ;  ;     ;     ;       ;        ;      ;          ;      ;  ;  ; 
;      ;        ;      ;   ;             ;   ;    ;   ;   ;     ;     ;      ;         ;       ;    ;    ;      ;  ;  ; 
;      ;     ;;;;;;;  ;;   ;;             ;;;      ;;;    ;     ;     ;      ;          ;;;     ;;;;     ;      ;  ;  ; 
;                                                                           ;                                           
;                                                                                                                       
;                                                                                                                       
; enforces context dependent rules:
; - break statement only into while
; - labelled terms only in the place of the redex
; PARAMS:
; t : the term
; any_1 : a boolean flag indicating if labelled terms must be removed (for
;         subterms that are not the redex
; any_2 : a boolean flag indicating if the actual term appears in place of an
;         expression
(define-metafunction ext-lang
  fix_cont_dep_rules : any any any -> any

  
  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;             ;               ;            
  ;             ;               ;            
  ;    ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
  ;   ;    ;    ;      ;   ;    ;     ;    ; 
  ;   ;         ;          ;    ;     ;      
  ;    ;;;;     ;      ;;;;;    ;      ;;;;  
  ;        ;    ;     ;    ;    ;          ; 
  ;   ;    ;    ;     ;   ;;    ;     ;    ; 
  ;    ;;;;      ;;;   ;;; ;     ;;;   ;;;;  
  ;                                          
  ;                                          
  ;                                          
  ;                                          

  ; simple fix to breaks outside of while loops
  [(fix_cont_dep_rules break any_1 any_2)
   (while true do break end)]

  [(fix_cont_dep_rules (return) any_1 any_2)
   (return)]
  
  [(fix_cont_dep_rules (side-condition
                        (return v ... e_1 e_2 ...)
                        (not (redex-match? ext-lang
                                           v_2
                                           (term e_1))))
                       any_1
                       any_2)
   (return v ...
           (fix_cont_dep_rules e_1 any_1 #t)
           (fix_cont_dep_rules e_2 #t #t) ...)]

  [(fix_cont_dep_rules ($statFCall e_1 (e_2 ...)) any_1 any_2)
   ($statFCall e_3 (e_4 ...))

   (where (e_3 (e_4 ...)) (fix_cont_dep_rules (e_1 (e_2 ...)) any_1 #t))]

  [(fix_cont_dep_rules ($statFCall e_1 : Name (e_2 ...)) any_1 any_2)
   ($statFCall e_3 : Name (e_4 ...))

   (where (e_3 (e_4 ...)) (fix_cont_dep_rules (e_1 (e_2 ...)) any_1 #t))]

  [(fix_cont_dep_rules (side-condition
                        (evar ... var_1 var_2 ... = e ...)
                        (not (redex-match? ext-lang evar_2 (term var_1))))
                       any _)
   (evar ... (fix_cont_dep_rules var_1 any #t)
             (fix_cont_dep_rules var_2 #t #t) ... =
             (fix_cont_dep_rules e #t #t) ...)]

  [(fix_cont_dep_rules (evar ... = v ... ) any_1 any_2)
   (evar ... = v ...)]

  [(fix_cont_dep_rules (side-condition
                        (evar ... = v ... e_1 e_2 ...)
                        (not (redex-match? ext-lang v_2 (term e_1))))
                       any_1 any_2)
   (evar ... = v ...
               (fix_cont_dep_rules e_1 any_1 #t)
               (fix_cont_dep_rules e_2 #t #t) ...)]

  [(fix_cont_dep_rules (do s end) any_1 any_2)
   (do (fix_cont_dep_rules s any_1 #f) end)]

  [(fix_cont_dep_rules (if e then s_1 else s_2 end) any_1 any_2)
   (if (fix_cont_dep_rules e any_1 #t) then
       (fix_cont_dep_rules s_1 #t #f)
       else
       (fix_cont_dep_rules s_2 #t #f) end)]

  [(fix_cont_dep_rules (while e do s end) any_1 any_2)
   (while (fix_cont_dep_rules e #t #t) do (fix_cont_dep_rules s #t #f) end)]

  [(fix_cont_dep_rules (local Name ... = v ... in s end) any_1 any_2)
   (local Name ... = v ... in (fix_cont_dep_rules s #t #f) end)]

  [(fix_cont_dep_rules (side-condition
                        (local Name ... = v ... e_1 e_2 ... in s end)
                        (not (redex-match? ext-lang
                                           v_2
                                           (term e_1)))) any_1 any_2)
   (local Name ... = v ...
                     (fix_cont_dep_rules e_1 any_1 #t)
                     (fix_cont_dep_rules e_2 #t #t) ... in
                     (fix_cont_dep_rules s #t #f) end)]

  [(fix_cont_dep_rules (sing_1 sing_2 sing_3 ...) any_1 any_2)
   ((fix_cont_dep_rules sing_1 any_1 #f)
    (fix_cont_dep_rules sing_2 #t #f)
    (fix_cont_dep_rules sing_3 #t #f) ...)]

  [(fix_cont_dep_rules ($err v) _ _)
   ($err v)]

  [(fix_cont_dep_rules ($iter e do s end) _ _)
   ($iter (fix_cont_dep_rules e #t #t) do (fix_cont_dep_rules s #t #f) end)]
  
  ;                                                                          
  ;                                                                          
  ;   ;;;             ;                                                      
  ;     ;             ;                                                      
  ;     ;             ;                         ;               ;            
  ;     ;             ;                         ;               ;            
  ;     ;       ;;;   ;;;;;            ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
  ;     ;      ;   ;  ;;  ;;          ;    ;    ;      ;   ;    ;     ;    ; 
  ;     ;          ;  ;    ;          ;         ;          ;    ;     ;      
  ;     ;      ;;;;;  ;    ;           ;;;;     ;      ;;;;;    ;      ;;;;  
  ;     ;     ;    ;  ;    ;               ;    ;     ;    ;    ;          ; 
  ;     ;     ;   ;;  ;;  ;;          ;    ;    ;     ;   ;;    ;     ;    ; 
  ;      ;;;   ;;; ;  ;;;;;            ;;;;      ;;;   ;;; ;     ;;;   ;;;;  
  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;                                                                          

  [(fix_cont_dep_rules (s (renv ...) LocalBody) #t _)
   \;]

  [(fix_cont_dep_rules (s (renv ...) LocalBody) #f _)
   ((fix_cont_dep_rules s #f #f) (renv ...) LocalBody)]

  [(fix_cont_dep_rules (s (renv ...) RetStat) #f _)
   ((fix_cont_dep_rules s #f #f) (renv ...) RetStat)]

  [(fix_cont_dep_rules (s (renv ...) RetStat) #t _)
   \;]

  ; TODO: in this case we are trying to fix a break even though it is no needed
  [(fix_cont_dep_rules (s Break) #f _)
   ((fix_cont_dep_rules s #f #f) Break)]
  
  [(fix_cont_dep_rules (s Break) #t _)
   \;]
  
  [(fix_cont_dep_rules (s Meta tid ...) #t #f)
   \;]
  
  [(fix_cont_dep_rules (s Meta tid ...) #f #f)
   (((1 \[ 2 \]) = 3) Meta tid ...)

   (side-condition (not (redex-match? ext-lang
                                      (((v_1 \[ v_2 \]) = v_3) ...
                                       ($statFCall v_4 (v_5 ...)) ...)
                                      (term (s)))))]
  
  ; simple fix
  [(fix_cont_dep_rules (s WrongKey tid_1 ...) #f #f)
   ; try indexing the environment
   ((((objr ,objStoreFirstLocation) \[ 1 \]) = 1) WrongKey tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     ; TODO: not checking membership of v_1 to
                                     ; tid_2
                                     ((tid_2 \[ v_1 \]) = v_2)
                                     (term s))))]

  [(fix_cont_dep_rules (s WrongKey tid_1 ...) #t #f)
   \;]
  
  [(fix_cont_dep_rules (s NonTable tid ...) #f #f)
   (((1 \[ 1 \]) = 1) NonTable tid ...)

   (side-condition (not (redex-match ext-lang
                                     ((v_1 \[ v_2 \]) = v_3)
                                     (term s))))]

  [(fix_cont_dep_rules (s NonTable tid_1 ...) #t #f)
   \;]
  
  [(fix_cont_dep_rules (s WFunCall tid_1 ...) #f #f)
   (($statFCall 1 ()) WFunCall tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     ($statFCall v_1 (v_2 ...))
                                     (term s))))]

  [(fix_cont_dep_rules (s WFunCall tid_1 ...) #t #f)
   \;]
  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;;  ;    ; 
  ;   ;    ;    ;;    ;    ;  ;      
  ;   ;;;;;;    ;;    ;    ;   ;;;;  
  ;   ;         ;;    ;    ;       ; 
  ;   ;;   ;   ;  ;   ;;  ;;  ;    ; 
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;                   ;              
  ;                   ;              
  ;                   ;              
  ;                                  

  [(fix_cont_dep_rules ($builtIn builtinserv ()) _ _)
   ($builtIn builtinserv ())]

  [(fix_cont_dep_rules (side-condition
                        ($builtIn builtinserv (v ... e_1 e_2 ...))
                        (not (redex-match? ext-lang
                                           v_2
                                           (term e_1))))
                       any _)
   ($builtIn builtinserv (v ...
                          (fix_cont_dep_rules e_1 any #t)
                          (fix_cont_dep_rules e_2 #t #t) ...))]
  
  [(fix_cont_dep_rules (function Name_1 (Name_2 ...) s end) _ _)
   (function Name_1 (Name_2 ...) (fix_cont_dep_rules s #t #f) end)]

  [(fix_cont_dep_rules (function Name_1 (Name_2 ... <<<) s end) _ _)
   (function Name_1 (Name_2 ... <<<) (fix_cont_dep_rules s #t #f) end)]

  [(fix_cont_dep_rules (v \[ e \]) any _)
   (v \[ (fix_cont_dep_rules e any #t) \])]

  [(fix_cont_dep_rules (e_1 \[ e_2 \]) any _)
   ((fix_cont_dep_rules e_1 any #t) \[ (fix_cont_dep_rules e_2 #t #t) \])]

  [(fix_cont_dep_rules (\( e \)) any _)
   (\( (fix_cont_dep_rules e any #t) \))]

  [(fix_cont_dep_rules (\{ efield ... \}) _ _)
   (\{ efield ... \})]

  [(fix_cont_dep_rules (side-condition
                        (\{ efield ... field_1 field_2 ... \})
                        (not (redex-match? ext-lang efield_2 (term field_1))))
                       any _)
   (\{ efield ...
       (fix_cont_dep_rules field_1 any #t)
       (fix_cont_dep_rules field_2 #t #t)... \})]

  ; table field with key defined
  [(fix_cont_dep_rules (\[ v \] = e) any _)
   (\[ v \] = (fix_cont_dep_rules e any #t))]
  
  [(fix_cont_dep_rules (\[ e_1 \] = e_2) any _)
   (\[ (fix_cont_dep_rules e_1 any #t) \] = (fix_cont_dep_rules e_2 #t #t))]

  [(fix_cont_dep_rules (v binop e) any _)
   (v binop (fix_cont_dep_rules e any #t))]

  [(fix_cont_dep_rules (e_1 binop e_2) any _)
   ((fix_cont_dep_rules e_1 any #t) binop (fix_cont_dep_rules e_2 #t #t))]

  [(fix_cont_dep_rules (unop e) any _)
   (unop (fix_cont_dep_rules e any #t))]

  [(fix_cont_dep_rules (< v ... >) _ _)
   (< v ... >)]

  [(fix_cont_dep_rules (side-condition
                        (< v ... e_1 e_2 ... >)
                        (not (redex-match? ext-lang v_2 (term e_1))))
                       any _)
   (< v ...
      (fix_cont_dep_rules e_1 any #t)
      (fix_cont_dep_rules e_2 #t #t) ... >)]

  [(fix_cont_dep_rules (v_1 (v_2 ...)) _ _)
   (v_1 (v_2 ...))]

  [(fix_cont_dep_rules (side-condition
                        (v_1 (v_2 ... e_1 e_2 ...))
                        (not (redex-match? ext-lang v_3 (term e_1))))
                       any _)
   (v_1 (v_2 ...
         (fix_cont_dep_rules e_1 any #t)
         (fix_cont_dep_rules e_2 #t #t) ...))]

  [(fix_cont_dep_rules (e_1 (e_2 ...)) any _)
   ((fix_cont_dep_rules e_1 any #t) ((fix_cont_dep_rules e_2 #t #t) ...))]
  
  [(fix_cont_dep_rules (e_1 : Name (e_2 ...)) any _)
   (e_3 : Name (e_4 ...))
   
   (where (e_3 (e_4 ...)) (fix_cont_dep_rules (e_1 (e_2 ...)) any #t))]

  
  ;                                                                  
  ;                                                                  
  ;   ;;;             ;                                              
  ;     ;             ;                                              
  ;     ;             ;                                              
  ;     ;             ;                                              
  ;     ;       ;;;   ;;;;;            ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;     ;      ;   ;  ;;  ;;          ;;  ;;   ;  ;   ;;  ;;  ;    ; 
  ;     ;          ;  ;    ;          ;    ;    ;;    ;    ;  ;      
  ;     ;      ;;;;;  ;    ;          ;;;;;;    ;;    ;    ;   ;;;;  
  ;     ;     ;    ;  ;    ;          ;         ;;    ;    ;       ; 
  ;     ;     ;   ;;  ;;  ;;          ;;   ;   ;  ;   ;;  ;;  ;    ; 
  ;      ;;;   ;;; ;  ;;;;;            ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;                                                   ;              
  ;                                                   ;              
  ;                                                   ;              
  ;                                                                  

  [(fix_cont_dep_rules (e ProtMD) #f _)
   (1 ProtMD)

   (side-condition (not (redex-match? ext-lang
                                      ((v) ...
                                       (\( e_2 \)) ...)
                                      (term (e)))))]

  [(fix_cont_dep_rules (e ProtMD v) #f _)
   (1 ProtMD)

   (side-condition (not (redex-match? ext-lang
                                      ((v_1 (v_2 ...)) ...

                                       ; successful fcall
                                       (s (renv ...) RetExp) ...
                                       (< v_3 ... >) ...

                                       ; problems in fcall
                                       (δbasic error v_4) ...
                                       ($err v_5)
                                       ((v_6 (v_7 ...)) WFunCall tid ...) ...
                                       ((v_8 (v_9 ...)) Meta tid ...) ...
                                       )
                                      (term (e)))))]

  [(fix_cont_dep_rules (e ProtMD v ...) #t _)
   nil]

  [(fix_cont_dep_rules (e Meta tid ...) #f #t)
   ((1 ()) Meta tid ...)

   (where (v ...
           <<< ...
           Name ...
           r ...
           ($err v_2) ...
           (< e_2 ... >) ...
           )
          
          (e))]

  ; simple fix
  [(fix_cont_dep_rules (e WrongKey tid_1 ...) #f #t)
   ; try indexing the environment
   (((objr ,objStoreFirstLocation) \[ 1 \]) WrongKey tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     ; TODO: not checking membership of v_1 to
                                     ; tid_2
                                     (tid_2 \[ v_1 \])
                                     (term e))))]

  [(fix_cont_dep_rules (e NonTable tid_1 ...) #f #t)
   ((1 \[ 1 \]) NonTable tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     (v_1 \[ v_2 \])
                                     (term e))))]

  [(fix_cont_dep_rules (e WFunCall tid_1 ...) #f #t)
   ((1 ()) WFunCall tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     (v_1 (v_2 ...))
                                     (term e))))]

  ; > is translated into <
  [(fix_cont_dep_rules ((e_1 > e_2) BinopWO tid_1 ...) #f _)
   (fix_cont_dep_rules ((e_1 < e_2) BinopWO tid_1 ...) #f #t)]

  [(fix_cont_dep_rules ((e_1 >= e_2) BinopWO tid_1 ...) #f _)
   (fix_cont_dep_rules ((e_1 <= e_2) BinopWO tid_1 ...) #f #t)]

  [(fix_cont_dep_rules (e BinopWO tid_1 ...) #f _)
   (("a" + "b") BinopWO tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     (v_1 binop v_2)
                                     (term e))))]

  [(fix_cont_dep_rules (e EqFail tid_1 ...) #f _)
   (("a" == 1) EqFail tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     (v_1 == v_2)
                                     (term e))))]

  [(fix_cont_dep_rules (e NegWrongOp tid_1 ...) #f _)
   ((- "a") NegWrongOp tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     (- v)
                                     (term e))))]

  [(fix_cont_dep_rules (e StrLenWrongOp tid_1 ...) #f _)
   ((\# 1) StrLenWrongOp tid_1 ...)

   (side-condition (not (redex-match ext-lang
                                     (\# v)
                                     (term e))))]

  [(fix_cont_dep_rules (e explabel tid_1 ...) #t _)
   ($err 1)]

  [(fix_cont_dep_rules (s (renv ...) RetExp) #t _)
   nil]

  [(fix_cont_dep_rules (s (renv ...) RetExp) #f _)
   ((fix_cont_dep_rules s #f #f) (renv ...) RetExp)]

  ; default
  [(fix_cont_dep_rules any _ _)
   any]
  )

; bound free variables and references
(define-metafunction ext-lang
  close_term_meta : any -> t
  
  ; no vararg id
  [(close_term_meta s_1)
   s_3
   
   ; fix breaks, labelled terms in wrong positions, etc
   (where s_2 (fix_cont_dep_rules s_1 #f #f))

   ; get free refs
   (where (any_1 any_2 ...) ,(remove-duplicates (term (fv s_2))))
   ; replace local variables identifiers by refs (later steps will bound this
   ; refs)
   (where s_3 (subst s_2 ((any_1 (ref 1)) (any_2 (ref 1)) ...)))]

  ; no free identifiers
  [(close_term_meta s)
   (fix_cont_dep_rules s #f #f)]

  [(close_term_meta e_1)
   e_3

   ; fix breaks, labelled terms in wrong positions, etc
   (where e_2 (fix_cont_dep_rules e_1 #f #t))
   
   (where (any_1 any_2 ...) ,(remove-duplicates (term (fv e_2))))
   ; replace local variables identifiers by refs (later steps will bound this
   ; refs)
   (where e_3 (subst e_2 ((any_1 (ref 1)) (any_2 (ref 1)) ...)))]

  ; no free identifiers
  [(close_term_meta e)
   (fix_cont_dep_rules e #f #t)]

  ; redex-check may have generated an ill-formed term
  [(close_term_meta any)
   \;]
  )

(define (close_term c)
  (term (close_term_meta (unquote c))))

(provide close_term)


; bound free variables and references; enforces well-formedness of domains and
; img of σ and θ; construct a valid configuration (σ : θ : s), ready for
; reduction with full-progs-rel
(define-metafunction ext-lang
  ; must receive (vsp ...) and (osp ...), and not σ and θ, since their domains
  ; could not be well-formed
  close_conf_meta  : ((vsp ...) : (osp ...) : t) -> (σ : θ : s)

  ; guarantee the presence of the global environment: load and loadstring assume
  ; its presence at the first position in σ
  ; no occurrence of ref 1
  [(close_conf_meta  (((refStdout String) ... (r v) ...)
                      : (osp ...)
                      : (in-hole E ($builtIn builtinserv (e ...)))))
   (close_conf_meta  (((refStdout String) ...
                       ((ref 1) (objr ,objStoreFirstLocation))
                       (r v) ...)
                      : (((objr ,objStoreFirstLocation) ((\{ \}) nil ⊥)) osp ...)
                      : (in-hole E ($builtIn builtinserv (e ...)))))

   (side-condition (member (term builtinserv)
                           (term (load loadstring))))

   (side-condition (not (redex-match? ext-lang
                                      (vsp_1 ... ((ref 1) v) vsp_2 ...)
                                      (term ((r v) ...)))))]

  ; occurrence of ref 1
  [(close_conf_meta  (((refStdout String) ...
                       (r_1 v_1) ...
                       ((ref 1) v_2)
                       (r_2 v_3) ...)
                      : (osp ...)
                      : (in-hole E ($builtIn builtinserv (e ...)))))
   (close_conf_meta  (((refStdout String) ...
                       ((ref 1) (objr ,objStoreFirstLocation))
                       (r_1 v_1) ...
                       (r_2 v_3) ...)
                      : (((objr ,objStoreFirstLocation) ((\{ \}) nil ⊥)) osp ...)
                      : (in-hole E ($builtIn builtinserv (e ...)))))

   (side-condition (member (term builtinserv)
                           (term (load loadstring))))

   (side-condition (not (is_tid? (term v_2))))]
  
  [(close_conf_meta  ((vsp ...) : (osp ...) : e_1))
   ; transform e_2 into a statement, ready for reduction with full-progs-rel
   (σ : θ : (return e_3))

   ; close var. identifiers
   (where e_2 (close_term_meta e_1))

   ; bound free val. refs, tids and cids;
   ; enforce well-formedness of dom((vsp ...)), dom((osp ...)) and images
   (where (σ θ e_3)  (close_fix_theta_sigma (vsp ...) (osp ...) e_2))]

  [(close_conf_meta  ((vsp ...) : (osp ...) : s_1))
   (σ : θ : s_3)

   ; close var. identifiers
   (where s_2 (close_term_meta s_1))

   ; bound free val. refs, tids and cids;
   ; enforce well-formedness of dom((vsp ...)), dom((osp ...)) and images
   (where (σ θ s_3)  (close_fix_theta_sigma (vsp ...) (osp ...) s_2))]
  )

; to interface with close_conf_meta, from terms of different relations
(define-metafunction ext-lang
  int_close_conf_meta : any -> (σ : θ : s)
  
  [(int_close_conf_meta t_1)
   (σ : θ : t_2)

   (where (σ : θ : t_2) (close_conf_meta (() : () : t_1)))]

  ; (vsp ...) instead of σ, to allow for ill-formed stores
  [(int_close_conf_meta ((vsp ...) : t_1))
   (σ : θ : t_2)

   (where (σ : θ : t_2) (close_conf_meta ((vsp ...) : () : t_1)))]

  
  [(int_close_conf_meta ((osp ...) : t_1))
   (σ : θ : t_2)

   (where (σ : θ : t_2) (close_conf_meta (() : (osp ...) : t_1)))]

  [(int_close_conf_meta ((vsp ...) : (osp ...) : t_1))
   (σ : θ : t_2)

   (where (σ : θ : t_2) (close_conf_meta ((vsp ...) : (osp ...) : t_1)))]

  ; deault case: redex-check generated something that is not in s ∪ e: we
  ; default it to the conf. () : () ;
  [(int_close_conf_meta any)
   (() : () : \;)]
  ) 

(define (close_conf c)
  (term (int_close_conf_meta (unquote c))))

(provide close_conf)