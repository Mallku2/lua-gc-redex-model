#lang racket
(require redex
         "../grammar.rkt"
         "./tablesMetafunctions.rkt"
         )

(define-metafunction ext-lang
  getTable : tid θ -> any

  [(getTable tid (osp_1 ... (tid (evaluatedtable any ...)) osp_2 ...))
   evaluatedtable]

  ; Allows calling getTable with a tid ∉ dom(θ). Simplifies the
  ; definition of meta-functions that iterate over θ
  [(getTable tid θ)
   nil]
  )

(provide getTable)

(define-metafunction ext-lang
  getMetaTable : tid θ -> any

  [(getMetaTable tid (osp_1 ... (tid (evaluatedtable any_1 any_2)) osp_2 ...))
   any_1]

  ; Allows calling getMetaTable with a tid ∉ dom(θ). Simplifies the
  ; definition of meta-functions that iterate over θ
  [(getMetaTable tid θ)
   nil]
  )

(provide getMetaTable)

; PRE : {tid in dom(θ)}
(define-metafunction ext-lang
  getPos : tid θ -> Number

  [(getPos tid (osp_1 ... (tid tableconstructor any Number) osp_2 ...))
   Number])

(provide getTable getMetaTable getPos)

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


; First location in the object store where values can be stored
; (because the locations since 1 to objStoreFirstLocation-1 are
; reserved to meta-tables of types different than table)
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