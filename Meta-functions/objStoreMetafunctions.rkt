#lang racket
(require redex
         "../grammar.rkt"
         "./tablesMetafunctions.rkt"
         )

; Access to an object store
; PRE : {}
; ret = (derefTheta θ ref)
; POS : {the correspondent value mapped to the reference}
(define-metafunction ext-lang
  derefTheta : θ objid -> object

  [(derefTheta (osp_1 ... (objid object) osp_2 ...) objid)
   object]

  ; Allows calling derefTheta with a objid ∉ dom(θ). Simplifies the
  ; definition of meta-functions that iterate over θ
  [(derefTheta objid θ)
   nil]
  )

(provide derefTheta)

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
  refBelongsToTheta? : tid θ -> any
  
  ; tid in dom(θ)
  [(refBelongsToTheta? tid (osp_1 ... (tid object) osp_2 ...))
   #t]
  
  ; Default case
  [(refBelongsToTheta? tid θ)
   #f])

(provide refBelongsToTheta?)


; First location in the object store where values can be stored
; (because the locations since 1 to objStoreFirstLocation-1 are
; reserved to meta-tables of types different than table)
(define objStoreFirstLocation 6)
(provide objStoreFirstLocation)

; Meta-function that generates a fresh tid. Its definition depends
; heavily on the fact that references are implicit generated only by this
; function and that we don't have any kind of garbage collection.
(define-metafunction ext-lang
  freshObjRef : θ -> tid
  ; Empty Store
  [(freshObjRef ())
   (objr ,objStoreFirstLocation)]
  
  ; An store with at least one reference
  ; Look for the table stored in the highest position 
  [(freshObjRef (osp ... ((objr Number_1) object) (cid functiondef) ...))
   (objr Number_2)
   (where Number_2 ,(+ (term Number_1) 1))]

  ; An store with no tid
  [(freshObjRef θ)
   (objr ,objStoreFirstLocation)]
  )

(provide freshObjRef)

(define-metafunction ext-lang
  freshClosId : θ -> cid
  ; Empty Store
  [(freshClosId ())
   (cl ,objStoreFirstLocation)]
  
  ; An store with at least one cid
  ; Look for the fdef stored in the highest position 
  [(freshClosId (osp_1 ... ((cl Number_1) functiondef) (tid intreptable) ...))
   (cl Number_2)
   (where Number_2 ,(+ (term Number_1) 1))]

  ; An store with no cid
  [(freshClosId θ)
   (cl ,objStoreFirstLocation)]

  )

(provide freshClosId)

(define-metafunction ext-lang
  domTheta : θ -> (tid ...)

  [(domTheta ())
   ()]

  [(domTheta ((tid_1 object_1) (tid_2 object_2) ...))
   ,(append (term (tid_1))
            (term (domTheta ((tid_2 object_2) ...))))]
  )

(provide domTheta)

(define-metafunction ext-lang
  delObj : θ v -> θ

  [(delObj (osp_1 ... (objid _) osp_2 ...) objid)
   (osp_1 ... osp_2 ...)]

  ; To allow calling delObj with nil instead of a tid
  [(delObj θ v)
   θ]
  )

(provide delObj)