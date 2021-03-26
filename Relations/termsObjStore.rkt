#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetaFunctions.rkt"
         "../Meta-functions/delta.rkt"
         "../Meta-functions/tablesMetaFunctions.rkt")

; Expressions that interact with the objects' store
(define terms-obj-store
  (reduction-relation
   ext-lang
   ;#:domain (θ : t)
   #:arrow -->θ

   ;                                                                                          
   ;                                                             ;                            
   ;                                                                                          
   ;                                                                                          
   ;                                                                                          
   ;    ;;;;   ;;  ;;  ;;;;;    ;;;;    ;;;;    ;;;;    ;;;;   ;;;      ;;;;   ; ;;;    ;;;;  
   ;   ;;  ;;   ;  ;   ;;  ;;   ;;  ;  ;;  ;;  ;    ;  ;    ;    ;     ;;  ;;  ;;   ;  ;    ; 
   ;   ;    ;    ;;    ;    ;   ;      ;    ;  ;       ;         ;     ;    ;  ;    ;  ;      
   ;   ;;;;;;    ;;    ;    ;   ;      ;;;;;;   ;;;;    ;;;;     ;     ;    ;  ;    ;   ;;;;  
   ;   ;         ;;    ;    ;   ;      ;            ;       ;    ;     ;    ;  ;    ;       ; 
   ;   ;;   ;   ;  ;   ;;  ;;   ;      ;;   ;  ;    ;  ;    ;    ;     ;;  ;;  ;    ;  ;    ; 
   ;    ;;;;   ;;  ;;  ;;;;;    ;       ;;;;    ;;;;    ;;;;   ;;;;;    ;;;;   ;    ;   ;;;;  
   ;                   ;                                                                      
   ;                   ;                                                                      
   ;                   ;                                                                      

   
   ; table creation
   [-->θ ((osp ...) : evaluatedtable)
        ; new table, not set for finalization
        ((osp ... (objref ((addKeys evaluatedtable) nil ⊥))) : objref)

        (where objref (freshObjRef (osp ...)))

        Table-Constr]
   
   ; table indexing
   [-->θ (θ : (objref \[ v_1 \]))
        (θ : v_2)

        E-IndexTable

        (where v_2 (δ rawget objref v_1 θ))

        (side-condition (not (equal? (term v_2)
                                     (term nil))))]
   
   ; abnormal situations
   [-->θ (θ : (objref \[ v \]))
        (θ : ((objref \[ v \])WrongKey))
        
        E-AlertKeyNotFound
        
        (where nil (δ rawget objref v θ))]
   
   [-->θ (θ : (v_1 \[ v_2 \]))
        (θ : ((v_1 \[ v_2 \]) NonTable))
        
        E-AlertNonTableIndexed
        
        ; v_1 is not a reference to a table
        (side-condition (not (equal? (term (δ type v_1))
                                     (term "table"))))]

   ; built-in services
   [-->θ (θ : ($builtIn builtinserv (v ...)))
        (θ : (δ builtinserv v ... θ))

        (side-condition (member (term builtinserv)
                                (term (; basic functions
                                       ipairs
                                       next
                                       pairs
                                       loadfile
                                       getmetatable
                                       tostring
                                       rawget
                                       rawlen
                                       ; package
                                       require
                                       ; table
                                       table.unpack
                                       ;string
                                       string.dump))))

        E-BuiltInThetaRead]

   [-->θ (θ_1 : ($builtIn builtinserv (v ...)))
        (θ_2 : any)

        (side-condition (member (term builtinserv)
                                (term (; basic functions
                                       setmetatable
                                       rawset
                                       ; debug
                                       debug.setmetatable))))
        
        (where (θ_2 any) (δ builtinserv v ... θ_1))

        E-BuiltInThetaWrite]

   ; Closure creation
   [-->θ ((osp ...) : functiondef_1)
        ; New table, not set for finalization
        ((osp ... (cid_2 functiondef_1)) : cid_2)

        E-ClosureCreation

        ; first time creating this closure
        (side-condition (not (redex-match? ext-lang
                                           
                                           (osp_1 ...
                                            (cid_1
                                             (side-condition
                                              functiondef_2
                                              (equal? (term functiondef_1)
                                                      (term functiondef_2))))
                                            osp_2 ...)

                                           (term (osp ...)))))

        (where cid_2 (freshClosId (osp ...)))
        ]

   [-->θ ((osp_1 ... (cid functiondef) osp_2 ...) : functiondef)
        ; No need to create a new closure
        ((osp_1 ... (cid functiondef) osp_2 ...) : cid)

        E-ClosureCreationCache]

   
;                                                                                  
;                                                                                  
;                                                                                  
;             ;               ;                                       ;            
;             ;               ;                                       ;            
;    ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;   ;;;;;;;  ;;;;   ; ;;;   ;;;;;;   ;;;;  
;   ;    ;    ;      ;   ;    ;     ;;  ;;  ;  ;  ; ;;  ;;  ;;   ;    ;     ;    ; 
;   ;         ;          ;    ;     ;    ;  ;  ;  ; ;    ;  ;    ;    ;     ;      
;    ;;;;     ;      ;;;;;    ;     ;;;;;;  ;  ;  ; ;;;;;;  ;    ;    ;      ;;;;  
;        ;    ;     ;    ;    ;     ;       ;  ;  ; ;       ;    ;    ;          ; 
;   ;    ;    ;     ;   ;;    ;     ;;   ;  ;  ;  ; ;;   ;  ;    ;    ;     ;    ; 
;    ;;;;      ;;;   ;;; ;     ;;;   ;;;;   ;  ;  ;  ;;;;   ;    ;     ;;;   ;;;;  
;                                                                                  
;                                                                                  
;                                                                                  
; Table assignment
   [-->θ (θ_1 : ((objref \[ v_1 \]) = v_2))
        (θ_2 : \;)
        
        ; this way of determining if v_1 is a key of objref is mentioned in
        ; Lua's reference manual.
        (where v_3 (δ rawget objref v_1 θ_1))
        
        ; The key belongs to the table
        (side-condition (not (equal? (term v_3)
                                     (term nil))))
        
        (where (θ_2 objref) (δ rawset objref v_1 v_2 θ_1))

        Table-Update]
   
   [-->θ (θ : ((objref \[ v_1 \]) = v_2))
        (θ : (((objref \[ v_1 \]) = v_2)WrongKey))
        
        ; The table doesn't have v_1 as key
        (where nil (δ rawget objref v_1 θ))

        Table-Update-WK]
   
   [-->θ (θ : ((v_1 \[ v_2 \]) = v_3))
        (θ : (((v_1 \[ v_2 \]) = v_3) NonTable))
        
        ; Determine if simplevalue is not an reference pointing to a table
        (side-condition (not (equal? (term (δ type v_1))
                                     (term "table"))))

        Table-Update-NT]
   ))

(provide terms-obj-store)
