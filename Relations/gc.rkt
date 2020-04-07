#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         "./fullProgs.rkt")

; Semantics of complete programs

(define gc-rel
  (reduction-relation
   ext-lang
   #:domain (σ : θ : s)
   
   ; full-progs-rel
   [--> (σ_1 : θ_1 : s_1)
        (σ_2 : θ_2 : s_2)
        
        E-FullProgsRel
        
        (where ((σ_2 : θ_2 : s_2)) ,(apply-reduction-relation
                                     full-progs-rel
                                     (term (σ_1 : θ_1 : s_1))))]

   ; GC
   [--> (σ_1 : θ_1 : (in-hole E e))
        (σ_2 : θ_2 : (in-hole E ((function $1 () (return e) end)
                                 ((cid (tid))))))

        E-GcfinalizationExp
        
        (where (σ_2 θ_2 (cid (tid)))
               (δ (collectgarbage
                   σ_1
                   θ_1
                   (in-hole E e))))
        ]

   [--> (σ_1 : θ_1 : (in-hole E s))
        (σ_2 : θ_2 : (in-hole E ((cid (tid))
                                 s)))

        E-GcfinalizationStat
        
        (where (σ_2 θ_2 (cid (tid)))
               (δ (collectgarbage
                   σ_1
                   θ_1
                   (in-hole E s))))
        ]

   [--> (σ_1 : θ_1 : s)
        (σ_2 : θ_2 : s)

        E-Gc
        
        (where (σ_2 θ_2 nil)
               (δ (collectgarbage σ_1 θ_1 s)))

        (side-condition (or (not (equal? (term σ_1) (term σ_2)))
                            (not (equal? (term θ_1) (term θ_2)))))
        ]
   ))

(provide gc-rel)