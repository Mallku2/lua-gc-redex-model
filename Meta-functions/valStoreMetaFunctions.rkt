#lang racket
(require redex
         "../grammar.rkt")

; Meta-function that generates a fresh reference in sigma. Its definition
; depends heavily on the fact that references are implicit generated only by
; this function and that we don't have any kind of garbage collection.
(define-metafunction ext-lang
  freshSigmaRef : σ -> r

  ; Empty Store
  [(freshSigmaRef ())
   (ref 1)]

  ; discard stdout
  [(freshSigmaRef ((refStdout v) vsp ...))
   (freshSigmaRef (vsp ...))]

  
  [(freshSigmaRef (((ref Number_1) v) ...))
   (ref Number_2)

   (where Number_2 ,(+ (argmax max (term (Number_1 ...))) 1))])

; To add simplevalues to the store
(define-metafunction ext-lang
  addSimpVal : σ (v ...) -> (σ (r ...))
  ; Base case
  [(addSimpVal (vsp ...) ())
   ((vsp ...) ())]
  
  ; Inductive case
  [(addSimpVal (vsp ...) (v_1 v_2 ...))
   (σ (r_1 r ...))

   (where r_1 (freshSigmaRef (vsp ...)))
   (where (σ (r ...)) (addSimpVal (vsp ... (r_1 v_1)) (v_2 ...)))])

(provide addSimpVal)