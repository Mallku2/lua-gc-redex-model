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
   
   ; Terms
   [--> (σ : θ : (in-hole E any_1))
        ; to obtain a well-formed concat of stats when E and any_2 are concat
        ; stats
        (σ : θ : (concat-stats E any_2))
        
        E-terms
        (where (any_2) ,(apply-reduction-relation terms-rel (term any_1)))
        ]
   
   ; Terms that interact with the value store
   [--> (σ_1 : θ : (in-hole E any_1))
        (σ_2 : θ : (in-hole E any_2))
        
        E-valStoreTerms
        
        (where ((σ_2 : any_2)) ,(apply-reduction-relation terms-val-store
                                                          (term
                                                           (σ_1 : any_1))))]
   
   ; Terms that interact with the object store
   [--> (σ : θ_1 : (in-hole E any_1))
        (σ : θ_2 : (in-hole E any_2))
        
        E-objStoreTerms
        (where ((θ_2 : any_2)) ,(apply-reduction-relation terms-obj-store
                                                          (term
                                                           (θ_1 : any_1))))]

   ; Terms that interact with both stores
   [--> (σ_1 : θ_1 : (in-hole E any_1))
        (σ_2 : θ_2 : (in-hole E any_2))
        
        E-valObjStoreTerms
        (where ((σ_2 : θ_2 : any_2)) ,(apply-reduction-relation
                                       terms-val-obj-store
                                       (term (σ_1 : θ_1 : any_1))))]
   
   ; Meta
   [--> (σ : θ_1 : (in-hole E any_1))
        (σ : θ_2 : (in-hole E any_2))
        
        E-meta
        (where ((θ_2 : any_2))
               ,(apply-reduction-relation meta
                                          (term (θ_1 : any_1))))]
   
   ; Error propagation
   [--> (σ : θ : (in-hole Enp ($err v)))
        (σ : θ : ($err v))
        
        E-errorPropagation
        (side-condition (not (eq? (term Enp)
                                  (term hole))))]

   ; GC
   [--> (σ_1 : θ_1 : (in-hole E ($builtIn collectgarbage (v ...))))
        (σ_2 : θ_2 : (in-hole E any))

        E-builtInCollectgarbage
        
        (where (σ_2 θ_2 any)
               (δ (collectgarbage
                   σ_1
                   θ_1
                   (in-hole E ($builtIn collectgarbage (v ...))))))
        ]
   ))

(provide full-progs-rel)
