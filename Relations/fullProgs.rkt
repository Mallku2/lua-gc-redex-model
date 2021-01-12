#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/grammarMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         "./terms.rkt"
         "./termsValStore.rkt"
         "./termsObjStore.rkt"
         "./termsValObjStore.rkt"
         "./meta.rkt")

; Semantics of complete programs

(define full-progs-rel
  (reduction-relation
   ext-lang
   #:domain (σ : θ : s)
   #:arrow ↦
   
   ; Terms
   [↦ (σ : θ : (in-hole E t_1))
      ; to obtain a well-formed concat of stats when E and t_2 are concat
      ; stats (ex.: result of rule AssignSplit)
      (σ : θ : (concat-stats E t_2))
        
      (where (t_2) ,(apply-reduction-relation terms-rel (term t_1)))

      E-terms]
   
   ; Terms that interact with the value store
   [↦ (σ_1 : θ : (in-hole E t_1))
      (σ_2 : θ : (in-hole E t_2))

      (where ((σ_2 : t_2)) ,(apply-reduction-relation terms-val-store
                                                      (term
                                                       (σ_1 : t_1))))

      E-valStoreTerms]
   
   ; Terms that interact with the object store
   [↦ (σ : θ_1 : (in-hole E t_1))
      (σ : θ_2 : (in-hole E t_2))

      (where ((θ_2 : t_2)) ,(apply-reduction-relation terms-obj-store
                                                      (term
                                                       (θ_1 : t_1))))

      E-objStoreTerms]

   ; Terms that interact with both stores
   [↦ (σ_1 : θ_1 : (in-hole E t_1))
      (σ_2 : θ_2 : (in-hole E t_2))

      (where ((σ_2 : θ_2 : t_2)) ,(apply-reduction-relation
                                   terms-val-obj-store
                                   (term (σ_1 : θ_1 : t_1))))

        
      E-valObjStoreTerms]
   
   ; Meta
   [↦ (σ : θ_1 : (in-hole E t_1))
      (σ : θ_2 : (in-hole E t_2))
        
      (where ((θ_2 : t_2))
             ,(apply-reduction-relation meta
                                        (term (θ_1 : t_1))))

      E-meta]
   
   ; Error propagation
   [↦ (σ : θ : (in-hole Enp ($err v)))
      (σ : θ : ($err v))
      
      (side-condition (not (eq? (term Enp)
                                (term hole))))

      E-errorPropagation]

   ; GC
   [↦ (σ_1 : θ_1 : (in-hole E ($builtIn collectgarbage (v ...))))
      (σ_2 : θ_2 : (in-hole E t))

      (where (σ_2 θ_2 t)
             (δ collectgarbage
                σ_1
                θ_1
                (in-hole E ($builtIn collectgarbage (v ...)))))

      E-builtInCollectgarbage]
   ))

(provide full-progs-rel)
