#lang racket
(require redex
         "../grammar.rkt")

; Determine if a reference belongs to the domain of a store
; PRE : {the store received satisfy the invariant of representation}
(define-metafunction ext-lang
  refBelongsTo : r σ -> any

  [(refBelongsTo r_1 (rst_1 ... (r_1 v) rst_2 ...))
   #t]

  ; Default
  [(refBelongsTo r_1 σ)
   #f]
  )

(provide refBelongsTo)

(define-metafunction ext-lang
  domSigma : σ -> (r ...)

  [(domSigma ())
   ()]

  [(domSigma ((r_1 v_1) (r_2 v_2) ...))
   ,(append (term (r_1))
            (term (domSigma ((r_2 v_2) ...))))])

(provide domSigma)

; Meta-function that generates a fresh reference in sigma. Its definition
; depends heavily on the fact that references are implicit generated only by
; this function and that we don't have any kind of garbage collection.
(define-metafunction ext-lang
  freshSigmaRef : σ -> r

  ; Empty Store
  [(freshSigmaRef ())
   (ref 1)]

  [(freshSigmaRef ((refStdout v)))
   (ref 1)]
  
  ; An store with at least one reference
  [(freshSigmaRef (rst ... ((ref Number_1) v)))
   (ref Number_2)

   (where Number_2 ,(+ (term Number_1) 1))]

  [(freshSigmaRef (rst ... ((ref Number_1) v_1) (refStdout v_2)))
   (ref Number_2)

   (where Number_2 ,(+ (term Number_1) 1))]

  )

; To add simplevalues to the store
(define-metafunction ext-lang
  addSimpVal : σ (v ...) -> (σ (r ...))
  ; Base case
  [(addSimpVal (rst ...) ())
   ((rst ...) ())]
  
  ; Inductive case
  [(addSimpVal (rst ...) (v_1 v_2 ...))
   (σ (r_1 r ...))

   (where r_1 (freshSigmaRef (rst ...)))
   (where (σ (r ...)) (addSimpVal (rst ... (r_1 v_1)) (v_2 ...)))])

(provide addSimpVal)

; {r belongs to dom(σ)}
(define-metafunction ext-lang
  derefSigma : σ r -> v

  [(derefSigma ((r_1 v_1) ... (r_2 v_2) (r_3 v_3) ...) r_2)
   v_2]
  )

(provide derefSigma)

; {r belongs to dom(σ)}
(define-metafunction ext-lang
  delVal : σ r -> σ

  [(delVal ((r_1 v_1) ... (r_2 v_2) (r_3 v_3) ...) r_2)
   ((r_1 v_1) ... (r_3 v_3) ...)]
  )

(provide delVal)