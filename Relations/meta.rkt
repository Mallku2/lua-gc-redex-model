#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         ; indexMetaTable
         "../Meta-functions/deltaBasic.rkt"
         "../Meta-functions/tablesMetafunctions.rkt")

; Expressions handled by the meta-table mechanism.

(define meta
  (reduction-relation
   ext-lang
   #:arrow -->meta
   ;#:domain (θ : t)
   
   ; Call over a non-function value
   [-->meta (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WrongFunCall))
        (θ : ($statFunCall ..._1 any (v_1 v_2 ...)))
        
        E-WrongStatFunCallWithHandler
        ; Determine if sv has a meta-table
        (where any (indexMetaTable v_1 
                                   "__call" 
                                   θ))

        (side-condition (not (is_false_cond? (term any))))]
   
   [-->meta (θ : (($statFunCall ... v_1 (v_2 ...)) WrongFunCall))
        (θ : ($err String))
        
        E-WrongStatFunCallNoHandler
        ; Determine if v_1 has a meta-table
        (where v_3 (indexMetaTable v_1 
                                   "__call" 
                                   θ))

        (side-condition (is_false_cond? (term v_3)))
        
        (where String (errmessage WrongFunCall (δ type v_1)))]

   [-->meta (θ : ((v_1 \[ v_2 \]) explabel))
        (θ : (cid (v_1 v_2)))
        
        E-KeyNotFoundWithHandlerNormal
        ; determine if v_1 has a meta-table
        (where cid (indexMetaTable v_1 "__index" θ))]
   
   [-->meta (θ : ((v_1 \[ v_2 \]) explabel))
        (θ : (v_3 \[ v_2 \]))
        
        E-KeyNotFoundWithHandlerRepeat
        ; determine if v_1 has a meta-table
        (where v_3 (indexMetaTable v_1 "__index" θ))
        
        (side-condition (not (or (is_nil? (term v_3))
                                 (eq? (term (δ type v_3))
                                       "function"))))]
   
   [-->meta (θ : ((tid \[ v \]) WrongKey))
        (θ : nil)
        
        E-KeyNotFoundNoHandler
        ; Determine if the table doesn't have a meta-table or
        ; its meta-table doesn't have a field with key "__index"
        (where nil (indexMetaTable tid
                                   "__index"
                                   θ))]
   
   [-->meta (θ : ((v_1 \[ v_2 \]) NonTable))
        (θ : ($builtIn error (String)))
        
        E-NonTableIndexedNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where nil (indexMetaTable v_1 
                                   "__index" 
                                   θ))
        
        (where String (errmessage NonTable
                                  (δ type v_1)))]

   [-->meta (θ : ((v_1 binop v_2) explabel))
        (θ : (v_3 (v_1 v_2)))
        
        E-ArithOpWrongOperandsWithHandler
        
        (side-condition (or (is_arithop? (term binop))
                            (is_strconcat? (term binop))))
        
        ; Determine if simplevalue efectively has a meta-table
        (where v_3 (getBinHandler v_1
                                  v_2
                                  (binopeventkey binop)
                                  θ))
        
        (side-condition (not (is_false_cond? (term v_3))))]
   
   [-->meta (θ : ((v_1 binop v_2) explabel))
        (θ : ($builtIn error (String)))
        
        E-ArithWrongOperandsNoHandler

        (side-condition (or (is_arithop? (term binop))
                            (is_strconcat? (term binop))))
        
        ; Determine if sv_1 or sv_2 efectively has a meta-table
        (where v_3 (getBinHandler v_1 v_2 (binopeventkey binop) θ))

        (side-condition (is_false_cond? (term v_3)))
        
        (where String (errmessage explabel 
                                  (δ type v_1)
                                  (δ type v_2)))]
   
   [-->meta (θ : ((- v_1 )NegWrongOp))
        (θ : (v_2 (v_1)))
        
        E-NegationWrongOperandWithHandler
        ; Determine if v_1 efectively has a meta-table
        (where v_2 (getUnaryHandler v_1 
                                    (unopeventkey -) 
                                    θ))
        
        (side-condition (not (is_false_cond? (term v_2))))]
   
   [-->meta (θ : ((- v_1) NegWrongOp))
        (θ : ($builtIn error (String)))
        
        E-NegationWrongOperandNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where v_2 (getUnaryHandler v_1
                                    (unopeventkey -)
                                    θ))

        (side-condition (is_false_cond? (term v_2)))
        
        (where String (errmessage NegWrongOp (δ type v_1)))]
   
    
   [-->meta (θ : ((\# v_1) StrLenWrongOp))
        (θ : (v_2 (v_1)))
        
        LenWrongOpHandler
        ; Determine if we have a handler for the operation
        (where v_2 (getUnaryHandler v_1 
                                    (unopeventkey \#) 
                                    θ))
        
        (side-condition (not (is_nil? (term v_2))))]
   
   [-->meta (θ : ((\# objref) StrLenWrongOp))
        (θ : (δ \# evaluatedtable))
        
        LenWrongOpTable
        ; Determine if we have a handler for the operation
        (where nil (getUnaryHandler objref
                                    (unopeventkey \#)
                                    θ))
        
        (where (osp_1 ... (objref (evaluatedtable any ...)) osp_2 ...)
               θ)]
   
   [-->meta (θ : ((\# v) StrLenWrongOp))
        (θ : ($builtIn error (String_2)))
        
        E-LenWrongOpNoHandler
        ; Determine if we have a handler for the operation
        (where nil (getUnaryHandler v 
                                    (unopeventkey \#)
                                    θ))
        
        (where String_1 (δ type v))
        
        (side-condition (not (equal? (term String_1) 
                                     "table")))
        
        (where String_2 (errmessage StrLenWrongOp String_1))]
   
   ; Abnormal expressions with relational operators
   [-->meta (θ : ((v_1 == v_2)EqFail))
        (θ : (v_3 (v_1 v_2)))
        
        E-EqualityFailWithHandler
        ; Determine the type of sv_1
        (where v_3 (getEqualHandler v_1 v_2 θ))
        (side-condition (not (is_false_cond? (term v_3))))]
   
   [-->meta (θ : ((v_1 == v_2)EqFail))
        (θ : false)
        
        E-EqualityFailNoHandler
        ; Determine the type of sv_1
        (where v_3 (getEqualHandler v_1 v_2 θ))
        
        (side-condition (is_false_cond? (term v_3)))]
   
   [-->meta (θ : ((v_1 relop v_2)OrdCompWrongOps))
        (θ : (v_3 (v_1 v_2)))
        
        E-OrdCompFailWithHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey relop) 
                                  θ))
        
        (side-condition (not (is_false_cond? (term v_3))))]
   
   [-->meta (θ : ((v_1 < v_2)OrdCompWrongOps))
        (θ : ($builtIn error (String)))
        
        E-LessThanFailNoHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey <) 
                                  θ))
        
        (side-condition (is_false_cond? (term v_3)))
        
        (where String (errmessage OrdCompWrongOps 
                                  (δ type v_1)
                                  (δ type v_2)))]
   
   [-->meta (θ : ((v_1 <= v_2)OrdCompWrongOps))
        (θ : (not (v_4 (v_2 v_1))))
        
        E-LessThanOrEqualFailWithAltHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey <=) 
                                  θ))
        
        (side-condition (is_false_cond? (term v_3)))

        (where v_4 (getBinHandler v_1
                                  v_2
                                  (binopeventkey <)
                                  θ))
        
        (side-condition (not (is_false_cond? (term v_4))))]
   
   [-->meta (θ : ((v_1 <= v_2)OrdCompWrongOps))
        (θ : ($builtIn error (String)))
        
        E-LessThanOrEqualFailNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey <=) 
                                  θ))
        
        (side-condition (is_false_cond? (term v_3)))

        (where v_4 (getBinHandler v_1
                                  v_2
                                  (binopeventkey <)
                                  θ))
        
        (side-condition (is_false_cond? (term v_4)))
        
        (where String (errmessage OrdCompWrongOps
                                  (δ type v_1)
                                  (δ type v_2)))]

   
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
   
   ; abnormal statements involving table field assignment
   [-->meta (θ : (((v_1 \[ v_2 \]) = v_3) statlabel))
        (θ : ($statFunCall cid (v_1 v_2 v_3)))
        
        E-FieldAssignWrongKeyNormal
        ; determine if v_1 has a meta-table with a handler
        (where cid (indexMetaTable v_1 "__newindex" θ))]
   
   [-->meta (θ : (((v_1 \[ v_2 \]) = v_3) statlabel))
        (θ : ((v_4 \[ v_2 \]) = v_3))
        
        E-FieldAssignWrongKeyRepeat
        ; determine if v_1 has a meta-table with a handler
        (where v_4 (indexMetaTable v_1 "__newindex" θ))
        ; determine if in that field we don't have a reference to a function...
        (side-condition (not (or (is_nil? (term v_4))
                                 (eq? (term (δ type v_4))
                                       (term "function")))))]

   ; add new field 
   [-->meta (θ_1 : (((tid \[ v_1 \]) = v_2) WrongKey))
        (θ_2 : any_2)

        E-FieldAssignWrongKeyNoHandler
        ; determine if the table has no meta-table or its meta-table
        ; doesn't have "__newindex" as a key
        (where nil (indexMetaTable tid "__newindex" θ_1))

        ; try to create new field [v_1] = v_2
        (where (θ_2 any_1) (δ rawset tid v_1 v_2 θ_1))

        ; any_2 should be either skip (rawset was successful) or $err v
        ; in this way we simplify rules
        (where any_2 ,(if (is_tid? (term any_1))
                          (term \;) ; success
                          (term any_1) ; any_1 is an error object: key was
                                       ; nil,nan
                          ))]
   
   [-->meta (θ : (((v_1 \[ v_2 \]) = v_3) NonTable))
        (θ : (δ error String))

        E-FieldAssignOverNonTableNoHandler

        ; determine if v_1 efectively has a meta-table
        (where nil (indexMetaTable v_1 "__newindex" θ))

        (where String ,(string-append "attempt to index a "
                                      (term (δ type v_1))
                                      " value"))]
;
   ))

(provide meta)
