#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/grammarMetaFunctions.rkt"
         "../Meta-functions/delta.rkt"
         "./terms.rkt"
         "./termsValStore.rkt"
         "./termsObjStore.rkt"
         "./termsValObjStore.rkt"
         "./meta.rkt")

; semantics of complete programs

(define full-progs-rel
  (reduction-relation
   ext-lang
   #:domain (σ : θ : t)
   #:arrow ↦
   
   ; terms
   [↦ (σ : θ : (in-hole E t_1))
      ; to obtain a well-formed concat of stats when E and t_2 are concat
      ; stats (ex.: result of rule AssignSplit)
      (σ : θ : (concat-stats E t_2))
        
      (where (t_2) ,(apply-reduction-relation terms-rel (term t_1)))

      FWD-Pure]
   
   ; terms that interact with the value store
   [↦ (σ_1 : θ : (in-hole E t_1))
      (σ_2 : θ : (in-hole E t_2))

      (where ((σ_2 : t_2)) ,(apply-reduction-relation terms-val-store
                                                      (term
                                                       (σ_1 : t_1))))

      FWD-σ]
   
   ; terms that interact with the object store
   [↦ (σ : θ_1 : (in-hole E t_1))
      (σ : θ_2 : (in-hole E t_2))

      (where ((θ_2 : t_2)) ,(apply-reduction-relation terms-obj-store
                                                      (term
                                                       (θ_1 : t_1))))

      FWD-θ]

   ; terms that interact with both stores
   [↦ (σ_1 : θ_1 : (in-hole E t_1))
      (σ_2 : θ_2 : (in-hole E t_2))

      (where ((σ_2 : θ_2 : t_2)) ,(apply-reduction-relation
                                   terms-val-obj-store
                                   (term (σ_1 : θ_1 : t_1))))

        
      FWD-σθ]
   
   ; meta
   [↦ (σ : θ_1 : (in-hole E (t_1 label objid ...)))
      (σ : θ_2 : (in-hole E t_2))
        
      (where ((θ_2 : t_2))
             ,(apply-reduction-relation meta
                                        (term (θ_1 : (t_1 label objid ...)))))

      FWD-Meta]

   [↦ (σ_1 : θ_1 : (in-hole E (t_1 Meta objid ...)))
      (σ_2 : θ_2 : (in-hole E (in-hole E_2 (t_2 label objid ...))))

      ; t_1 is labelled
      (where ((σ_2 : θ_2 : (in-hole E_2 (t_2 label))))
             ,(apply-reduction-relation full-progs-rel
                                        (term (σ_1 : θ_1 : t_1))))

      E-metaOneStep]

   [↦ (σ_1 : θ_1 : (in-hole E (t_1 Meta objid ...)))
      (σ_2 : θ_2 : (in-hole E t_3))

      (where ((σ_2 : θ_2 : t_3))
             ,(apply-reduction-relation full-progs-rel
                                        (term (σ_1 : θ_1 : t_1))))

      (side-condition (not (redex-match? ext-lang
                                         (in-hole E_2 (t_4 label_2))
                                         (term t_3))))

      E-metaEnd]
   
   ; Error propagation
   [↦ (σ : θ : (in-hole Enp ($err v)))
      (σ : θ : ($err v))
      
      (side-condition (not (eq? (term Enp)
                                (term hole))))

      FWD-Error]

   ; GC
   [↦ (σ_1 : θ_1 : (in-hole E ($builtIn collectgarbage (v ...))))
      (σ_2 : θ_2 : (in-hole E t))

      (where (σ_2 θ_2 t)
             (δ collectgarbage
                v ...
                σ_1
                θ_1
                (in-hole E ($builtIn collectgarbage (v ...)))))

      FWD-GC]
   ))

(provide full-progs-rel)
