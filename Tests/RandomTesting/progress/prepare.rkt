#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Meta-functions/delta.rkt"
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
  free_tids_sigma : (vsp ...) (osp ...) -> (tid ...)

  [(free_tids_sigma () _)
   ()]

  [(free_tids_sigma ((r tid_1) vsp ...) (osp_1 ... (tid_1 any) osp_2 ...))
   (free_tids_sigma (vsp ...) (osp_1 ... (tid_1 any) osp_2 ...))
   
   (where (tid_2 ...) (free_tids_sigma (vsp ...)
                                       (osp_1 ... (tid_1 any) osp_2 ...)))]

  [(free_tids_sigma ((r tid_1) vsp ...) (osp ...))
   ; {tid ∉ dom(osp ...)}
   (tid_1 tid_2 ...)
   
   (where (tid_2 ...) (free_tids_sigma (vsp ...) (osp ...)))]

  ; {vsp_1 ≠ (r, tid)}
  [(free_tids_sigma (vsp_1 vsp_2 ...) (osp ...))
   (free_tids_sigma (vsp_2 ...) (osp ...))])


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


(define-metafunction ext-lang
  free_clids_sigma : (vsp ...) (osp ...) -> (cid ...)

  [(free_clids_sigma () _)
   ()]

  [(free_clids_sigma ((r cid_1) vsp ...) (osp_1 ... (cid_1 any) osp_2 ...))
   (free_clids_sigma (vsp ...) (osp_1 ... (cid_1 any) osp_2 ...))
   
   (where (cid_2 ...) (free_clids_sigma (vsp ...)
                                       (osp_1 ... (cid_1 any) osp_2 ...)))]

  [(free_clids_sigma ((r cid_1) vsp ...) (osp ...))
   ; {cid ∉ dom(osp ...)}
   (cid_1 cid_2 ...)
   
   (where (cid_2 ...) (free_clids_sigma (vsp ...) (osp ...)))]

  ; {vsp_1 ≠ (r, cid)}
  [(free_clids_sigma (vsp_1 vsp_2 ...) (osp ...))
   (free_clids_sigma (vsp_2 ...) (osp ...))])

; bound free tids, cids from a given conf; enforces well-formedness of the domain
; and img of given stores
(define-metafunction ext-lang
  close_fix_theta_sigma : σ (osp ...) t -> (σ θ t)

  [(close_fix_theta_sigma (vsp ...) (osp_1 ...) t_1)
   (σ_2 θ t_2)

   ; get free val. refs from t and σ
   (where (r_1 ...) (free_val_refs (vsp ...) t_1))
   (where (r_2 ...) (free_val_refs_theta (vsp ...) (osp_1 ...)))
   ; remove repeated refs.
   (where (r_3 ...) ,(remove-duplicates (term (r_1 ... r_2 ...))))
   
   ; add dummy vals. and bound free refs
   (where σ_1 (vsp ... (r_3 1) ... ))
   
   ; get free tids, cids from σ, θ and t
   (where (tid_1 ...) (free_tids (osp_1 ...) t_1))
   (where (tid_2 ...) (free_tids_theta (osp_1 ...)))
   (where (tid_3 ...) (free_tids_sigma σ_1 (osp_1 ...)))
   (where (tid_4 ...) ,(remove-duplicates (term (tid_1 ... tid_2 ... tid_3 ...))))
   
   (where (cid_1 ...) (free_clids (osp_1 ...) t_1))
   (where (cid_2 ...) (free_clids_theta (osp_1 ...)))
   (where (cid_3 ...) (free_clids_sigma σ_1 (osp_1 ...)))
   (where (cid_4 ...) ,(remove-duplicates (term (cid_1 ... cid_2 ... cid_3 ...))))
   
   ; add dummy tables and closures to bound tids and cids
   (where (osp_2 ...) (osp_1 ...
                       (tid_4 ((\{ \}) nil ⊥)) ...
                       (cid_4 (function x () \; end)) ...))
   
   ; ensure well-formedness of dom((osp_2 ...)) and img((osp_2 ...)))
   (where (θ ((objid_1 objid_2) ...)) (fix_theta_dom_img (osp_2 ...)))
   ; apply the expected substitutions in t_1 and σ_1
   (where t_2 (subst t_1 ((objid_1 objid_2) ...)))
   (where σ_2 (substSigma σ_1 ((objid_1 objid_2) ...)))]
  )

; redex-check could generate terms with refs (objr number_1) and (cl number_1)
; fix_theta_dom implements a simple fix to that
; it also enforces well-formedness of the img
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

  ; discard fields with nil or nan key, or nil value
  [(fix_tableconstructor_aux ((\[ v_1 \] = v_2) field ...))
   (fix_tableconstructor_aux (field ...))

   (side-condition (or (is_nil? (term v_1))
                       (equal? (term v_1)
                               +nan.0)
                       (is_nil? (term v_2))))]

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
;
; simple fix to the "break outside of while" problem
(define-metafunction ext-lang
  [(fix_break (in-hole C_1 break))
   (fix_break (in-hole C_1 (while 1 do break end)))

   (side-condition (not (redex-match? ext-lang
                                      (in-hole C_2 (while e do C_3 end))
                                      (term C_1))))]

  [(fix_break t)
   t

   (side-condition (println "------------------"))])


; bound free variables and references
(define-metafunction ext-lang
  close_term_meta : any -> t
  
  ; no vararg id
  [(close_term_meta s)
   (fix_break (local Name_1 Name_2 ... = nil in s end))

   ; close local variables
   (where (Name_1 Name_2 ...) ,(remove-duplicates (term (fv s))))]

  ; there is a vararg id
  [(close_term_meta s)
   (fix_break (local dummyVar = (function dummyF (<<<) s end) in \; end))

   ; only a vararg
   (where (<<<) ,(remove-duplicates (term (fv s))))]

  ; vararg id plus other var. id.
  [(close_term_meta s)
   (fix_break
    (local dummyVar = (function dummy (any_1 ... any_2 ... <<<) s end) in \;
      end))

   ; {# (any_1 ... any_2 ...) > 0}
   (where (any_1 ... <<< any_2 ...) ,(remove-duplicates (term (fv s))))]

  ; no free identifiers
  [(close_term_meta s)
   (fix_break s)]

  ; no vararg id
  [(close_term_meta e)
   (fix_break (function $dummy (Name_1 Name_2 ...) (local x = e in \; end) end))

   (where (Name_1 Name_2 ...) ,(remove-duplicates (term (fv e))))]

  ; there is a vararg id
  [(close_term_meta e)
   (fix_break (function $dummy (any_1 ... any_2 ... <<<)
                        (local x = e in \; end) end))

   (where (any_1 ... <<< any_2 ...) ,(remove-duplicates (term (fv e))))]

  ; no free identifiers
  [(close_term_meta e)
   (fix_break e)]

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
                       ((ref 1) (objr 1))
                       (r v) ...)
                      : (((objr 1) ((\{ \}) nil ⊥)) osp ...)
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
                       ((ref 1) (objr 1))
                       (r_1 v_1) ...
                       (r_2 v_3) ...)
                      : (((objr 1) ((\{ \}) nil ⊥)) osp ...)
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