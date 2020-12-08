#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Meta-functions/substitution.rkt"
         )

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

; PRE : {any ∈ s ∪ e}
(define-metafunction ext-lang
  free_val_refs : σ any -> (r ...)

  ; discard refStdout
  [(free_val_refs ((refStdout String) (r v) ...) any)
   (free_val_refs ((r v) ...) any)
   ]
  
  [(free_val_refs ((r_1 v) ...) any)
   (r_3 ...)

   (where (r_2 ...) ,(get_val_refs (term any)))
   (where (r_3 ...) ,(remove* (term (r_1 ...)) (term (r_2 ...))))
   ]
  )

(provide free_val_refs)

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
  free_tids : θ any -> (tid ...)

  [(free_tids ((any_1 object) ...) any_2)
   (tid_2 ...)

   (where (tid_1 ...) ,(get_tids (term any_2)))
   (where (tid_2 ...) ,(remove* (term (any_1 ...)) (term (tid_1 ...))))
   ]
  )

(provide free_tids)

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

; PRE : {any ∈ s ∪ e}
(define-metafunction ext-lang
  free_clids : θ any -> (cid ...)

  [(free_clids ((any_1 object) ...) any_2)
   (cid_2 ...)

   (where (cid_1 ...) ,(get_clids (term any_2)))
   (where (cid_2 ...) ,(remove* (term (any_1 ...)) (term (cid_1 ...))))
   ]
  )

(provide free_clids)

;; transform a given s into a term from block
;(define-metafunction ext-lang
;  convert2_block : s -> block
;  ; we only take care of the problematic cases
;  [(convert2_block ($statFunCall ))]
;  )

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

; bound free variables and references; enforce well-formedness of domains of
; σ and θ
(define-metafunction ext-lang
  close_conf_meta  : (σ : θ : t) -> (σ : θ : t)
  
  [(close_conf_meta  (σ_1 : θ_1 : t_1))
   (σ_2 : θ_2 : t_2)

   ; close var. identifiers
   (where t_2 (close_term_meta t_1))

   ; enforce well-formedness of dom(σ_1)
   (where (vsp ...) (fix_sigma_dom σ_1))
   ; close free val. refs
   (where (r ...) ,(remove-duplicates (term (free_val_refs (vsp ...) t_2))))
   ; add dummy vals
   (where σ_2 (vsp ... (r 1) ...))

   ; enforce well-formedness of dom(θ_1) and img
   (where (osp ...) (fix_theta_dom_img θ_1))
   ; close free tids and cids
   (where (tid ...) ,(remove-duplicates (term (free_tids (osp ...) t_2))))
   (where (cid ...) ,(remove-duplicates (term (free_clids (osp ...) t_2))))
   ; add dummy tables and closures
   (where θ_2 (osp ...
               (tid ((\{ \}) nil ⊥)) ...
               (cid (function x () \; end)) ...))
   ])

(define (close_conf c)
  (term (close_conf_meta (unquote c))))

(provide close_conf)

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

(provide fix_sigma_dom)

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

(provide fix_theta_dom_img)

; enforces a well-formed dom(θ) by replacing each reference with a
; reference that is unique; it also enforces well-formedness of the img
(define-metafunction ext-lang
  fix_theta_dom_img_aux : θ natural θ -> θ

  [(fix_theta_dom_img_aux () natural θ)
   ()]

  [(fix_theta_dom_img_aux (((objref natural_1) (tableconstructor_1 any_1 pos))
                       osp_1 ...) natural_2 θ)
   (((objref natural_2)
     ; in order to bound free ids in tableconstructor_1, close_term_meta
     ; returned a functiondef; we put it into a new table
     ((\{ (\[ 1 \] = functiondef) \}) any_1 pos)) osp_2 ...)

   (where natural_3 ,(+ 1 (term natural_2)))
   (where (osp_2 ...) (fix_theta_dom_img_aux (osp_1 ...) natural_3))

   ; table constructor must be well formed; note that, if any_1 is a ref
   ; not in dom(θ), later steps will add it to θ
   (where functiondef (close_term_meta tableconstructor_1))]

  [(fix_theta_dom_img_aux (((objref natural_1) (tableconstructor any pos))
                       osp_1 ...) natural_2 θ)
   (((objref natural_2)
     (tableconstructor any pos)) osp_2 ...)

   (where natural_3 ,(+ 1 (term natural_2)))
   (where (osp_2 ...) (fix_theta_dom_img_aux (osp_1 ...) natural_3))

   ; table constructor must be well formed; note that, if any_1 is a ref
   ; not in dom(θ), later steps will add it to θ
   (where tableconstructor (close_term_meta tableconstructor))]

  [(fix_theta_dom_img_aux (((cl natural_1) functiondef_1)
                       osp_1 ...) natural_2 θ)
   (((cl natural_2) functiondef_2) osp_2 ...)

   (where natural_3 ,(+ 1 (term natural_2)))
   (where (osp_2 ...) (fix_theta_dom_img_aux (osp_1 ...) natural_3))

   ; functiondef_1 must be well formed
   (where functiondef_2 (close_term_meta functiondef_1))]
  )
