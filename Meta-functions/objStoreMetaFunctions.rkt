#lang racket
(require redex
         "../grammar.rkt")

; Determine if a reference belongs to the domain of a store
; PRE : {the store received satisfy the invariant of representation}
(define-metafunction ext-lang
  refBelongsToTheta? : any θ -> any
  
  ; tid in dom(θ)
  [(refBelongsToTheta? tid (osp_1 ... (tid object) osp_2 ...))
   #t]

  ; cid in dom(θ)
  [(refBelongsToTheta? cid (osp_1 ... (cid object) osp_2 ...))
   #t]
  
  ; Default case
  [(refBelongsToTheta? _ _)
   #f])

(provide refBelongsToTheta?)

; first location in the object store where values can be stored:
; locations from 2 to objStoreFirstLocation-1 are
; reserved to meta-tables of types different than table
(define objStoreFirstLocation 6)
(provide objStoreFirstLocation)

; meta-function that generates a fresh tid
(define-metafunction ext-lang
  freshObjRef : θ -> tid
  ; empty Store
  [(freshObjRef ())
   (objr ,objStoreFirstLocation)]
  
  ; an store with at least one object
  [(freshObjRef (((any Number) object) ...))
   (objr Number_2)
   
   (where Number_2 ,(+ 1 (argmax max (term (Number ...)))))]
  )

(provide freshObjRef)

(define-metafunction ext-lang
  freshClosId : θ -> cid
  ; empty Store
  [(freshClosId ())
   (cl ,objStoreFirstLocation)]
  
  ; an store with at least one object
  [(freshClosId (((any Number) object) ...))
   (cl Number_2)
   
   (where Number_2 ,(+ 1 (argmax max (term (Number ...)))))]
  )

(provide freshClosId)