#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Meta-functions/substitution.rkt"
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
  free_val_refs : σ t -> (r ...)

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
  free_val_refs_theta : σ θ -> (r ...)

  ; discard refStdout
  [(free_val_refs_theta σ ())
   ()]
  
  [(free_val_refs_theta σ ((_ functiondef) osp ...))
   (r_3 ...)

   (where (r_1 ...) (free_val_refs σ functiondef))
   (where (r_2 ...) (free_val_refs_theta σ (osp ...)))
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))]

  [(free_val_refs_theta σ ((_ (tableconstructor _ _)) osp ...))
   (r_3 ...)

   (where (r_1 ...) (free_val_refs σ tableconstructor))
   (where (r_2 ...) (free_val_refs_theta σ (osp ...)))
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))]
  )

; redex-check tends to generate stores with repeated references
; (typically (ref 0)); fix_sigma_dom implements a simple fix to that
(define-metafunction ext-lang
  fix_sigma_dom : σ θ t -> (σ θ t)

  ; empty store or refStdout
  [(fix_sigma_dom ((refStdout String) ...) θ t)
   (((refStdout String) ...) θ t)]

  ; with or without refStdout
  [(fix_sigma_dom ((refStdout String) ... ((ref natural) v) vsp_1 ...) θ_1 t_1)
   (((refStdout String) ... vsp_2 ...) θ_2 t_2)
   
   (where ((vsp_2 ...) θ_2 t_2) (fix_sigma_dom_aux (((ref natural) v) vsp_1 ...)
                                                   natural
                                                   θ_1 t_1))]
  )

; enforces a well-formed dom(σ) by replacing each reference with a
; reference that is unique; applies the corresponding substitution of references
; in θ and t
; PRE : {refStdout ∉ dom(σ)}
(define-metafunction ext-lang
  fix_sigma_dom_aux : σ natural θ t -> (σ θ t)

  [(fix_sigma_dom_aux () natural θ t)
   (() θ t)]

  [(fix_sigma_dom_aux ((r_1 v) vsp_1 ...) natural_1 θ_1 t_1)
   ((((ref natural_1) v) vsp_2 ...) θ_3 t_3)

   (where natural_2 ,(+ 1 (term natural_1)))
   
   ; replace the old ref by the new one in θ_1 and t_1
   (where θ_2 (substTheta θ_1 ((r_1 (ref natural_1)))))
   (where t_2 (subst t_1 ((r_1 (ref natural_1)))))
   
   (where ((vsp_2 ...) θ_3 t_3)
          (fix_sigma_dom_aux (vsp_1 ...) natural_2 θ_2 t_2))]
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
  free_tids : θ t -> (tid ...)

  [(free_tids ((any object) ...) t)
   (tid_2 ...)

   (where (tid_1 ...) ,(get_tids (term t)))
   (where (tid_2 ...) ,(remove-duplicates
                        (remove* (term (any ...)) (term (tid_1 ...)))))]
  )

(provide free_tids)

; free tids in θ
(define-metafunction ext-lang
  free_tids_theta : θ -> (tid ...)

  [(free_tids_theta θ)
   (free_tids_theta_aux θ θ)]
  )

(define-metafunction ext-lang
  free_tids_theta_aux : θ θ -> (tid ...)

  [(free_tids_theta_aux θ ())
   ()]

  ; a functiondef shouldn't have a tid, but redex-check (and the grammar) allow
  ; them
  [(free_tids_theta_aux θ ((_ functiondef) osp ...))
   (tid_3 ...)

   (where (tid_1 ...) (free_tids θ functiondef))
   (where (tid_2 ...) (free_tids_theta_aux θ (osp ...)))
   (where (tid_3 ...) ,(remove-duplicates (term (tid_1 ... tid_2 ...))))]

  ; table with meta-table set
  [(free_tids_theta_aux θ ((_ (tableconstructor any _)) osp ...))
   (tid_4 ...)

   (where (tid_1 ...) (free_tids θ tableconstructor))
   ; check if the metatable is actually in θ or not
   (where (tid_2 ...) (free_tids θ any))
   (where (tid_3 ...) (free_tids_theta_aux θ (osp ...)))
   (where (tid_4 ...) ,(remove-duplicates (term (tid_1 ...
                                                 tid_2 ...
                                                 tid_3 ...))))]
  )

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
  free_clids : θ t -> (cid ...)

  [(free_clids ((any object) ...) t)
   (cid_2 ...)

   (where (cid_1 ...) ,(get_clids (term t)))
   (where (cid_2 ...) ,(remove-duplicates
                        (remove* (term (any ...)) (term (cid_1 ...)))))]
  )

; free cids in θ
(define-metafunction ext-lang
  free_clids_theta : θ -> (cid ...)

  [(free_clids_theta θ)
   (free_clids_theta_aux θ θ)]
  )

(define-metafunction ext-lang
  free_clids_theta_aux : θ θ -> (cid ...)

  [(free_clids_theta_aux θ ())
   ()]

  ; a functiondef shouldn't have a cid, but redex-check (and the grammar) allow
  ; them
  [(free_clids_theta_aux θ ((_ functiondef) osp ...))
   (cid_3 ...)

   (where (cid_1 ...) (free_clids θ functiondef))
   (where (cid_2 ...) (free_clids_theta_aux θ (osp ...)))
   (where (cid_3 ...) ,(remove-duplicates (term (cid_1 ... cid_2 ...))))]

  ; table without meta-table set
  [(free_clids_theta_aux θ ((_ (tableconstructor _ _)) osp ...))
   (cid_3 ...)

   (where (cid_1 ...) (free_clids θ tableconstructor))
   (where (cid_2 ...) (free_clids_theta_aux θ (osp ...)))
   (where (cid_3 ...) ,(remove-duplicates (term (cid_1 ... cid_2 ...))))]
  )

; bound free tids, cids from a given conf; enforces well-formedness of the domain
; and img of given stores
(define-metafunction ext-lang
  close_fix_theta_sigma : σ θ t -> (σ θ t)

  [(close_fix_theta_sigma (vsp ...) (osp_1 ...) t_1)
   (σ_2 θ_2 t_3)

   ; get free val. refs from t and σ
   (where (r_1 ...) (free_val_refs (vsp ...) t_1))
   (where (r_2 ...) (free_val_refs_theta (vsp ...) (osp_1 ...)))
   
   ; add dummy vals. and bound free refs
   (where σ_1 (vsp ... (r_1 1) ... (r_2 1) ...))
   ; enforce well-formedness of dom(σ_1), apply expected substitution of refs
   ; in θ and t
   (where (σ_2 (osp_2 ...) t_2) (fix_sigma_dom σ_1 (osp_1 ...) t_1))
   
   ; get free tids, cids from t and θ
   (where (tid_1 ...) (free_tids (osp_2 ...) t_2))
   (where (tid_2 ...) (free_tids_theta (osp_2 ...)))
   
   (where (cid_1 ...) (free_clids (osp_2 ...) t_2))
   (where (cid_2 ...) (free_clids_theta (osp_2 ...)))
   
   ; add dummy tables and closures to bound tids and cids
   (where θ_1 (osp_2 ...
               (tid_1 ((\{ \}) nil ⊥)) ...
               (tid_2 ((\{ \}) nil ⊥)) ...
               
               (cid_1 (function x () \; end)) ...
               (cid_2 (function x () \; end)) ...))
   
   ; enforce well-formedness of dom(θ_1) and img; applies the expected
   ; substitutions in t
   (where (θ_2 t_3) (fix_theta_dom_img θ_1 t_2))]
  )

; redex-check tends to generate stores with repeated references
; (typically (objr 0)); fix_theta_dom implements a simple fix to that;
; it also enforces well-formedness of the img
(define-metafunction ext-lang
  fix_theta_dom_img : θ t -> (θ t)

  [(fix_theta_dom_img () t)
   (() t)]

  [(fix_theta_dom_img (((any natural) object) osp ...) t)
   (fix_theta_dom_img_aux () (((any natural) object) osp ...) natural t)]
   
  )

; enforces a well-formed dom(θ) by replacing each reference with a
; reference that is unique; it also enforces well-formedness of the img
; PRE : {θ_1 ++ θ_2 = θ}
(define-metafunction ext-lang
  fix_theta_dom_img_aux : θ θ natural t -> (θ t)

  [(fix_theta_dom_img_aux θ () natural t)
   (θ t)]

  [(fix_theta_dom_img_aux θ_1
                          (((objr natural_1) (tableconstructor_1 any_1 pos))
                           osp_1 ...)
                          natural_2 t_1)
   (fix_theta_dom_img_aux
    (osp_2 ... ((objr natural_2)
                ; in order to bound free ids in tableconstructor_1,
                ; close_term_meta returned a functiondef; we put it into a new
                ; table
                ((\{ (\[ 1 \] = functiondef) \}) any_2 pos)))
    (osp_3 ...) natural_3 t_2)

   ; substitute (objr natural_1) by (objr natural_2) in t_1 and θ = θ_1 ++ ...
   (where t_2 (subst t_1 (((objr natural_1) (objr natural_2)))))
   (where (osp_2 ...) (substTheta θ_1 (((objr natural_1) (objr natural_2)))))
   ; substitution into (tableconstructor_1 any_1 pos)
   (where tableconstructor_2 (substExp tableconstructor_1
                                       (((objr natural_1) (objr natural_2)))))
   (where any_2 (substExp any_1
                          (((objr natural_1) (objr natural_2)))))
   ; substitution in what remains of θ
   (where (osp_3 ...) (substTheta (osp_1 ...)
                                  (((objr natural_1) (objr natural_2)))))

   ; table constructor must be well formed; note that, if any_1 is a ref
   ; not in dom(θ), it will add it to θ when bounding free refs;
   ; the following holds when tableconstructor has free variables id
   (where functiondef (close_term_meta tableconstructor_2))

    ; next pos in θ
   (where natural_3 ,(+ 1 (term natural_2)))]

  
  [(fix_theta_dom_img_aux θ_1
                          (((objr natural_1) (tableconstructor_1 any_1 pos))
                           osp_1 ...)
                          natural_2 t_1)
   (fix_theta_dom_img_aux
    (osp_2 ... ((objr natural_2) (tableconstructor_2 any_2 pos)))
    (osp_3 ...) natural_3 t_2)

   ; substitute (objr natural_1) by (objr natural_2) in t_1 and θ = θ_1 ++ ...
   (where t_2 (subst t_1 (((objr natural_1) (objr natural_2)))))
   (where (osp_2 ...) (substTheta θ_1 (((objr natural_1) (objr natural_2)))))
   ; substitution into (tableconstructor_1 any_1 pos)
   (where tableconstructor_2 (substExp tableconstructor_1
                                       (((objr natural_1) (objr natural_2)))))
   (where any_2 (substExp any_1
                          (((objr natural_1) (objr natural_2)))))
   ; substitution in what remains of θ
   (where (osp_3 ...) (substTheta (osp_1 ...)
                                  (((objr natural_1) (objr natural_2)))))

   ; table constructor must be well formed; note that, if any_1 is a ref
   ; not in dom(θ), it will add it to θ when bounding free refs;
   ; the following holds when tableconstructor does not have free vars
   (where tableconstructor_2 (close_term_meta tableconstructor_2))

    ; next pos in θ
   (where natural_3 ,(+ 1 (term natural_2)))]

  
  [(fix_theta_dom_img_aux θ_1
                          (((cl natural_1) functiondef_1)
                           osp_1 ...)
                          natural_2 t_1)
   (fix_theta_dom_img_aux
    (osp_2 ... ((cl natural_2) functiondef_3))
    (osp_3 ...) natural_3 t_2)

   ; substitute (objr natural_1) by (objr natural_2) in t_1 and θ = θ_1 ++ ...
   (where t_2 (subst t_1 (((objr natural_1) (objr natural_2)))))
   (where (osp_2 ...) (substTheta θ_1 (((objr natural_1) (objr natural_2)))))
   ; substitution into functiondef_1
   (where functiondef_2 (substExp functiondef_1
                                  (((objr natural_1) (objr natural_2)))))
   ; substitution in what remains of θ
   (where (osp_3 ...) (substTheta (osp_1 ...)
                                  (((objr natural_1) (objr natural_2)))))

   ; table constructor must be well formed; note that, if any_1 is a ref
   ; not in dom(θ), it will add it to θ when bounding free refs;
   ; the following holds when tableconstructor does not have free vars
   (where functiondef_3 (close_term_meta functiondef_2))

    ; next pos in θ
   (where natural_3 ,(+ 1 (term natural_2)))]
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
;                                                                                                                       

; bound free variables and references
(define-metafunction ext-lang
  close_term_meta : any -> t
  
  ; no vararg id
  [(close_term_meta s)
   (local Name_1 Name_2 ... = nil in s end)
   ; TODO: in this case, we must transform s into a block

   ; close local variables
   (where (Name_1 Name_2 ...) ,(remove-duplicates (term (fv s))))
   ]

  ; there is a vararg id
  [(close_term_meta s)
   (local dummyVar = (function dummyF (<<<) s end) in \; end)
   ; TODO: in this case, we must transform s into a block

   ; only a vararg
   (where (<<<) ,(remove-duplicates (term (fv s))))
   ]

  ; vararg id plus other var. id.
  [(close_term_meta s)
   (local any_1 ... any_2 ... = (function dummy (<<<) s end) in
     \;
     end)
   ; TODO: in this case, we must transform s into a block

   ; {# (any_1 ... any_2 ...) > 0}
   (where (any_1 ... <<< any_2 ...) ,(remove-duplicates (term (fv s))))
   ]

  ; no free identifiers
  [(close_term_meta s)
   s]

  ; no vararg id
  [(close_term_meta e)
   (function $dummy (Name_1 Name_2 ...) (local x = e in \; end) end)

   (where (Name_1 Name_2 ...) ,(remove-duplicates (term (fv e))))
   ]

  ; there is a vararg id
  [(close_term_meta e)
   (function $dummy (any_1 ... any_2 ... <<<)
             (local x = e in \; end) end)

   (where (any_1 ... <<< any_2 ...) ,(remove-duplicates (term (fv e))))
   ]

  ; no free identifiers
  [(close_term_meta e)
   e]

  ; redex-check may have generated an ill-formed term
  [(close_term_meta any)
   \;]
  )

(define (close_term c)
  (term (close_term_meta (unquote c))))

(provide close_term)


; bound free variables and references; enforce well-formedness of domains and
; img of σ and θ; construct a valid configuration (σ : θ : s), ready for
; reduction with full-progs-rel
(define-metafunction ext-lang
  close_conf_meta  : (σ : θ : t) -> (σ : θ : s)
  
  [(close_conf_meta  (σ_1 : θ_1 : e_1))
   ; transform e_2 into a statement, ready for reduction with full-progs-rel
   (σ_2 : θ_2 : (if e_3 then \; else \; end))

   ; close var. identifiers
   (where e_2 (close_term_meta e_1))

   ; bound free val. refs, tids and cids;
   ; enforce well-formedness of dom(σ_1), dom(θ_1) and images
   (where (σ_2 θ_2 e_3)  (close_fix_theta_sigma σ_1 θ_1 e_2))]

  [(close_conf_meta  (σ_1 : θ_1 : s_1))
   (σ_2 : θ_2 : s_3)

   ; close var. identifiers
   (where s_2 (close_term_meta s_1))

   ; bound free val. refs, tids and cids;
   ; enforce well-formedness of dom(σ_1), dom(θ_1) and images
   (where (σ_2 θ_2 s_3)  (close_fix_theta_sigma σ_1 θ_1 s_2))]
  )

; to interface with close_conf_meta, from terms of different relations
(define-metafunction ext-lang
  int_close_conf_meta : any -> (σ : θ : s)
  
  [(int_close_conf_meta t_1)
   (σ : θ : t_2)
   
   (where (σ : θ : t_2) (close_conf_meta (() : () : t_1)))]

  [(int_close_conf_meta (σ_1 : t_1))
   (σ_2 : θ : t_2)
   
   (where (σ_2 : θ : t_2) (close_conf_meta (σ_1 : () : t_1)))]

  [(int_close_conf_meta (θ_1 : t_1))
   (σ : θ_2 : t_2)
   
   (where (σ : θ_2 : t_2) (close_conf_meta (() : θ_1 : t_1)))]

  [(int_close_conf_meta (σ_1 : θ_1 : t_1))
   (σ_2 : θ_2 : t_2)
   
   (where (σ_2 : θ_2 : t_2) (close_conf_meta (σ_1 : θ_1 : t_1)))]

  ; deault case: redex-check generated something that is not in s ∪ e: we
  ; default it to the conf. () : () ;
  [(int_close_conf_meta any)
   (() : () : \;)]
  ) 

(define (close_conf c)
  (term (int_close_conf_meta (unquote c))))

(provide close_conf)