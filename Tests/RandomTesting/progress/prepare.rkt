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

; bound free variables and references; enforce well-formedness of domains of
; σ and θ
(define-metafunction ext-lang
  ; TODO: redex-check may generate a tuple (σ : θ : any), where any ∉ s, even
  ; though the template is (σ : θ : s)
  close : (σ : θ : any) -> (σ : θ : any)
  
  ; no vararg id
  [(close (σ_1 : θ_1 : s))
   (σ_2 : θ_2 : (local Name_1 Name_2 ... = nil in s end))

   ; close local variables
   (where (Name_1 Name_2 ...) ,(remove-duplicates (term (fv s))))

   ; enforce well-formedness of dom(σ_1)
   (where (vsp ...) (fix_sigma_dom σ_1))
   ; close free val. refs
   (where (r ...) ,(remove-duplicates (term (free_val_refs (vsp ...) s))))
   ; add dummy vals
   (where σ_2 (vsp ... (r 1) ...))

   ; enforce well-formedness of dom(θ_1)
   (where (osp ...) (fix_theta_dom θ_1))
   ; close free tids and cids
   (where (tid ...) ,(remove-duplicates (term (free_tids (osp ...) s))))
   (where (cid ...) ,(remove-duplicates (term (free_clids (osp ...) s))))
   ; add dummy tables and closures
   (where θ_2 (osp ...
               (tid ((\{ \}) nil ⊥)) ...
               (cid (function x () \; end)) ...))
   ]

  ; there is a vararg id
  [(close (σ_1 : θ_1 : s))
   (σ_2 : θ_2 : (local any_1 ... any_2 ... = nil in (function dummy (<<<) s end)
              end))

   (where (any_1 ... <<< any_2 ...) ,(remove-duplicates (term (fv s))))

    ; enforce well-formedness of dom(σ_1)
   (where (vsp ...) (fix_sigma_dom σ_1))
   ; close free val. refs
   (where (r ...) ,(remove-duplicates (term (free_val_refs (vsp ...) s))))
   ; add dummy vals
   (where σ_2 (vsp ... (r 1) ...))

   ; enforce well-formedness of dom(θ_1)
   (where (osp ...) (fix_theta_dom θ_1))
   ; close free tids and cids
   (where (tid ...) ,(remove-duplicates (term (free_tids (osp ...) s))))
   (where (cid ...) ,(remove-duplicates (term (free_clids (osp ...) s))))
   ; add dummy tables and closures
   (where θ_2 (osp ...
               (tid ((\{ \}) nil ⊥)) ...
               (cid (function x () \; end)) ...))
   ]

  ; no free identifier
  [(close (σ_1 : θ_1 : s))
   (σ_2 : θ_2 : s)

    ; enforce well-formedness of dom(σ_1)
   (where (vsp ...) (fix_sigma_dom σ_1))
   ; close free val. refs
   (where (r ...) ,(remove-duplicates (term (free_val_refs (vsp ...) s))))
   ; add dummy vals
   (where σ_2 (vsp ... (r 1) ...))

   ; enforce well-formedness of dom(θ_1)
   (where (osp ...) (fix_theta_dom θ_1))
   ; close free tids and cids
   (where (tid ...) ,(remove-duplicates (term (free_tids (osp ...) s))))
   (where (cid ...) ,(remove-duplicates (term (free_clids (osp ...) s))))
   ; add dummy tables and closures
   (where θ_2 (osp ...
               (tid ((\{ \}) nil ⊥)) ...
               (cid (function x () \; end)) ...))
   ])

(define (close_term c)
  (term (close (unquote c))))

(provide close_term)

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
; (typically (objr 0)); fix_theta_dom implements a simple fix to that
(define-metafunction ext-lang
  fix_theta_dom : θ -> θ

  [(fix_theta_dom ())
   ()]

  [(fix_theta_dom (((any natural) object) osp ...))
   (fix_theta_dom_aux (((any natural) object) osp ...) natural)]
   
  )

(provide fix_theta_dom)

; enforces a well-formed dom(θ) by replacing each reference with a
; reference that is unique
(define-metafunction ext-lang
  fix_theta_dom_aux : θ natural -> θ

  [(fix_theta_dom_aux () natural)
   ()]

  [(fix_theta_dom_aux (((any natural_1) object) osp_1 ...) natural_2)
   (((any natural_2) object) osp_2 ...)

   (where natural_3 ,(+ 1 (term natural_2)))
   (where (osp_2 ...) (fix_theta_dom_aux (osp_1 ...) natural_3))]
  )
