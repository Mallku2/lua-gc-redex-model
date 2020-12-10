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

; free val refs in θ (from the environment of closures)
(define-metafunction ext-lang
  free_val_refs_theta : σ θ -> (r ...)

  ; discard refStdout
  [(free_val_refs_theta σ ())
   ()]
  
  [(free_val_refs_theta σ ((_ functiondef) osp ...))
   (r_3 ...)

   (where (r_1 ...) (free_val_refs σ functiondef))
   (where (r_2 ...) (free_val_refs σ (osp ...)))
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))]

  [(free_val_refs_theta σ ((_ (evaluatedtable _ _)) osp ...))
   (r_3 ...)

   (where (r_1 ...) (free_val_refs σ evaluatedtable))
   (where (r_2 ...) (free_val_refs σ (osp ...)))
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))]
  )

; bounds free val refs from given t; enforces well-formedness of the domain
; and img of a given σ
(define-metafunction ext-lang
  close_fix_sigma : σ t -> σ

  [(close_fix_sigma (vsp ...) t)
   σ_2

   ; get free val. refs
   (where (r ...) (free_val_refs (vsp ...) t))
   ; add dummy vals. and bound free refs
   (where σ_1 (vsp ... (r 1) ...))
   ; enforce well-formedness of dom(σ_1)
   (where σ_2 (fix_sigma_dom σ_1))]
   
  )

; redex-check tends to generate stores with repeated references
; (typically (ref 0)); fix_sigma_dom implements a simple fix to that
(define-metafunction ext-lang
  fix_sigma_dom : σ -> σ

  [(fix_sigma_dom ())
   ()]

  [(fix_sigma_dom ((refStdout String)))
   ((refStdout String))]

  [(fix_sigma_dom ((refStdout String) ((ref natural) v) vsp_1 ...))
   ((refStdout String) vsp_2 ...)
   
   (where (vsp_2 ...) (fix_sigma_dom_aux (((ref natural) v) vsp_1 ...)
                                             natural))]

  [(fix_sigma_dom (((ref natural) v) vsp ...))
   (fix_sigma_dom_aux (((ref natural) v) vsp ...) natural)
   ]
  )

; enforces a well-formed dom(σ) by replacing each reference with a
; reference that is unique
; PRE : {refStdout ∉ dom(σ)}
(define-metafunction ext-lang
  fix_sigma_dom_aux : σ natural -> σ

  [(fix_sigma_dom_aux () natural)
   ()]

  [(fix_sigma_dom_aux ((r_1 v) vsp_1 ...) natural)
   (((ref natural) v) vsp_2 ...)

   (where natural_2 ,(+ 1 (term natural)))
   (where (vsp_2 ...) (fix_sigma_dom_aux (vsp_1 ...) natural_2))]
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

; closures ids not bound in θ
(define-metafunction ext-lang
  free_clids : θ t -> (cid ...)

  [(free_clids ((any object) ...) t)
   (cid_2 ...)

   (where (cid_1 ...) ,(get_clids (term t)))
   (where (cid_2 ...) ,(remove-duplicates
                        (remove* (term (any ...)) (term (cid_1 ...)))))]
  )


; bound free tids, cids from a given t; enforces well-formedness of the domain
; and img of a given θ, and bounds free val. refs. from the domain of its
; closures
(define-metafunction ext-lang
  close_fix_theta_sigma : σ θ t -> (σ θ)

  [(close_fix_theta_sigma (vsp ...) (osp ...) t)
   (σ θ_2)

   ; get free tids, cids and val. refs.
   (where (tid ...) (free_tids (osp ...) t))
   (where (cid ...) (free_clids (osp ...) t))
   (where (r ...) (free_val_refs_theta (vsp ...) (osp ...)))
   
   ; add dummy tables and closures to bound tids and cids
   (where θ_1 (osp ...
               (tid ((\{ \}) nil ⊥)) ...
               (cid (function x () \; end)) ...))

   ; add dummy vals. and bound free refs
   (where σ (vsp ... (r 1) ...))
   
   ; enforce well-formedness of dom(θ_1) and img
   (where θ_2 (fix_theta_dom_img θ_1))]
  )

; redex-check tends to generate stores with repeated references
; (typically (objr 0)); fix_theta_dom implements a simple fix to that;
; it also enforces well-formedness of the img
(define-metafunction ext-lang
  fix_theta_dom_img : θ -> θ

  [(fix_theta_dom_img ())
   ()]

  [(fix_theta_dom_img (((any natural) object) osp ...))
   (fix_theta_dom_img_aux (((any natural) object) osp ...) natural
                      (((any natural) object) osp ...))]
   
  )

; enforces a well-formed dom(θ) by replacing each reference with a
; reference that is unique; it also enforces well-formedness of the img
(define-metafunction ext-lang
  fix_theta_dom_img_aux : θ natural θ -> θ

  [(fix_theta_dom_img_aux () natural θ)
   ()]

  [(fix_theta_dom_img_aux (((objr natural_1) (tableconstructor_1 any_1 pos))
                       osp_1 ...) natural_2 θ)
   (((objr natural_2)
     ; in order to bound free ids in tableconstructor_1, close_term_meta
     ; returned a functiondef; we put it into a new table
     ((\{ (\[ 1 \] = functiondef) \}) any_1 pos)) osp_2 ...)

   (where natural_3 ,(+ 1 (term natural_2)))
   (where (osp_2 ...) (fix_theta_dom_img_aux (osp_1 ...) natural_3 θ))

   ; table constructor must be well formed; note that, if any_1 is a ref
   ; not in dom(θ), later steps will add it to θ
   (where functiondef (close_term_meta tableconstructor_1))]

  [(fix_theta_dom_img_aux (((objr natural_1) (tableconstructor any pos))
                       osp_1 ...) natural_2 θ)
   (((objr natural_2)
     (tableconstructor any pos)) osp_2 ...)

   (where natural_3 ,(+ 1 (term natural_2)))
   (where (osp_2 ...) (fix_theta_dom_img_aux (osp_1 ...) natural_3 θ))

   ; table constructor must be well formed; note that, if any_1 is a ref
   ; not in dom(θ), later steps will add it to θ
   (where tableconstructor (close_term_meta tableconstructor))]

  [(fix_theta_dom_img_aux (((cl natural_1) functiondef_1) osp_1 ...)
                          natural_2 θ)
   (((cl natural_2) functiondef_2) osp_2 ...)

   (where natural_3 ,(+ 1 (term natural_2)))
   (where (osp_2 ...) (fix_theta_dom_img_aux (osp_1 ...) natural_3 θ))

   ; functiondef_1 must be well formed
   (where functiondef_2 (close_term_meta functiondef_1))]
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
   \;
   ]
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
   (σ_3 : θ_2 : (if e_2 then \; else \; end))

   ; close var. identifiers
   (where e_2 (close_term_meta e_1))

   ; enforce well-formedness of dom(σ_1) and bound free val. refs
   (where σ_2 (close_fix_sigma σ_1 e_2))

   ; enforce well-formedness of dom(θ_1) and bound free val. refs
   ; look for free val refs in the environment of closures in θ_1
   ; and bound them in σ
   (where (σ_3 θ_2)  (close_fix_theta_sigma σ_2 θ_1 e_2))]

  [(close_conf_meta  (σ_1 : θ_1 : s_1))
   (σ_3 : θ_2 : s_2)

   ; close var. identifiers
   (where s_2 (close_term_meta s_1))

   ; enforce well-formedness of dom(σ_1) and bound free val. refs
   (where σ_2 (close_fix_sigma σ_1 s_2))

   ; enforce well-formedness of dom(θ_1) and bound free val. refs
   ; look for free val refs in the environment of closures in θ_1
   ; and bound them in σ
   (where (σ_3 θ_2)  (close_fix_theta_sigma σ_2 θ_1 s_2))]
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
   (() : () : \;)

   ; print ill formed term
   (side-condition (begin
                     (println (term any))
                     #t))]
  ) 

(define (close_conf c)
  (term (int_close_conf_meta (unquote c))))

(provide close_conf)