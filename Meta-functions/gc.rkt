#lang racket

; GC mechanism's utilities

(require redex
         "../grammar.rkt"
         "./finalization.rkt"
         "./weakTables.rkt")

; gc cycle with weak-tables-aware finalization, but no cleaning of weak tables
; specification from fig. 7 with changes to turn it aware of weak tables
(define-metafunction ext-lang
  gcFinWeakAware : s σ θ -> (σ θ e)

  [(gcFinWeakAware s σ_1 θ_1)
   (σ_2 θ_2 e_2)

   (where σ_2 (cleanSigma σ_1 θ_1 s))

   (where (θ_2 e_1) (cleanThetaWeak σ_1 θ_1 s))

   ; if cleanThetaWeak returned a table for finalization (e_1 = tid)
   ; look for, and invoke, the finalizer
   (where e_2 (invokeFin e_1 θ_2))])

(provide gcFinWeakAware)


; complete definition of a gc cycle, including weak-tables-aware finalization
; and cleaning of weak tables, fig. 12
(define-metafunction ext-lang
  gcFinWeak : s σ θ -> (σ θ e)

  [(gcFinWeak s σ_1 θ_1)
   (σ_2 θ_4 e)

   (where (σ_2 θ_2 e) (gcFinWeakAware s σ_1 θ_1))

   ; clean weak tables
   (where θ_3 (cleanWeakTables σ_2 θ_2 s))

   ; if e is an invocation to a finalizer over a given tid, prevent it
   ; from being marked again for finalization
   (where θ_4 (preventFin e θ_3))])

(provide gcFinWeak)