#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         "../Meta-functions/tablesMetafunctions.rkt")

; Expressions handled by the meta-table mechanism.

(define meta
  (reduction-relation
   ext-lang
   #:domain (side-condition (θ : any) (is_term? (term any)))
   
   ; Call over a non-function value
   [--> (θ : ((v_1 (v_2 ...))WrongFunCall))
        (θ : (any  (v_1 v_2 ...)))
        
        E-WrongFunCallWithHandler
        ; Determine if sv has a meta-table
        (where any (indexMetaTable v_1 
                                   "__call" 
                                   θ))
        
        (side-condition (not (is_false_cond? (term any))))]
   
   [--> (θ : ((v_1 (v_2 ...))WrongFunCall))
        (θ : ($err String))
        
        E-WrongFunCallNoHandler
        ; Determine if v_1 has a meta-table
        (where v_3 (indexMetaTable v_1 
                                   "__call" 
                                   θ))

        (side-condition (is_false_cond? (term v_3)))
        
        (where String (errmessage WrongFunCall (δ (type v_1))))]

   [--> (θ : (($statFunCall v_1 (v_2 ...))WrongFunCall))
        (θ : ($statFunCall any (v_1 v_2 ...)))
        
        E-WrongStatFunCallWithHandler
        ; Determine if sv has a meta-table
        (where any (indexMetaTable v_1 
                                   "__call" 
                                   θ))
        
        (side-condition (not (is_false_cond? (term any))))]
   
   [--> (θ : (($statFunCall v_1 (v_2 ...))WrongFunCall))
        (θ : ($err String))
        
        E-WrongStatFunCallNoHandler
        ; Determine if v_1 has a meta-table
        (where v_3 (indexMetaTable v_1 
                                   "__call" 
                                   θ))

        (side-condition (is_false_cond? (term v_3)))
        
        (where String (errmessage WrongFunCall (δ (type v_1))))]

   [--> (θ : ((v_1 \[ v_2 \]) explabel))
        (θ : (v_3 (v_1 v_2)))
        
        E-KeyNotFoundWithHandlerNormal
        ; Determine if the table efectively has a meta-table

        (where v_3 (indexMetaTable v_1
                                   "__index"
                                   θ))
        
        (side-condition (equal? (term (δ (type v_3))) 
                                "function"))
        ]
   
   [--> (θ : ((v_1 \[ v_2 \])explabel))
        (θ : (v_3 \[ v_2 \]))
        
        E-KeyNotFoundWithHandlerRepeat
        ; Determine if the table efectively has a meta-table
        (where v_3 (indexMetaTable v_1
                                   "__index"
                                   θ))
        
        (side-condition (not (or (is_nil? (term v_3))
                                 (eq? (term (δ (type v_3))) 
                                       "function"))))]
   
   [--> (θ : ((objref \[ v \]) WrongKey))
        (θ : nil)
        
        E-KeyNotFoundNoHandler
        ; Determine if the table doesn't have a meta-table or
        ; its meta-table doesn't have a field with key "__index"
        (where nil (indexMetaTable objref
                                   "__index"
                                   θ))]
   
   [--> (θ : ((v_1 \[ v_2 \])NonTable))
        (θ : ($builtIn error (String)))
        
        E-NonTableIndexedNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where nil (indexMetaTable v_1 
                                   "__index" 
                                   θ))
        
        (where String (errmessage NonTable
                                  (δ (type v_1))))]

   [--> (θ : ((v_1 binop v_2)explabel))
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
   
   [--> (θ : ((v_1 binop v_2)explabel))
        (θ : ($builtIn error (String)))
        
        E-ArithWrongOperandsNoHandler

        (side-condition (or (is_arithop? (term binop))
                            (is_strconcat? (term binop))))
        
        ; Determine if sv_1 or sv_2 efectively has a meta-table
        (where v_3 (getBinHandler v_1 v_2 (binopeventkey binop) θ))

        (side-condition (is_false_cond? (term v_3)))
        
        (where String (errmessage explabel 
                                  (δ (type v_1)) 
                                  (δ (type v_2))))]
   
   [--> (θ : ((- v_1)NegWrongOp))
        (θ : (v_2 (v_1)))
        
        E-NegationWrongOperandWithHandler
        ; Determine if v_1 efectively has a meta-table
        (where v_2 (getUnaryHandler v_1 
                                    (unopeventkey -) 
                                    θ))
        
        (side-condition (not (is_false_cond? (term v_2))))]
   
   [--> (θ : ((- v_1)NegWrongOp))
        (θ : ($builtIn error (String)))
        
        E-NegationWrongOperandNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where v_2 (getUnaryHandler v_1
                                    (unopeventkey -)
                                    θ))

        (side-condition (is_false_cond? (term v_2)))
        
        (where String (errmessage NegWrongOp (δ (type v_1))))]
   
    
   [--> (θ : ((\# v_1)StrLenWrongOp))
        (θ : (v_2 (v_1)))
        
        LenWrongOpHandler
        ; Determine if we have a handler for the operation
        (where v_2 (getUnaryHandler v_1 
                                    (unopeventkey \#) 
                                    θ))
        
        (side-condition (not (is_nil? (term v_2))))]
   
   [--> (θ : ((\# objref)StrLenWrongOp))
        (θ : (δ (\# evaluatedtable)))
        
        LenWrongOpTable
        ; Determine if we have a handler for the operation
        (where nil (getUnaryHandler objref
                                    (unopeventkey \#)
                                    θ))
        
        (where (osp_1 ... (objref (evaluatedtable any ...)) osp_2 ...)
               θ)]
   
   [--> (θ : ((\# v)StrLenWrongOp))
        (θ : ($builtIn error (String_2)))
        
        E-LenWrongOpNoHandler
        ; Determine if we have a handler for the operation
        (where nil (getUnaryHandler v 
                                    (unopeventkey \#)
                                    θ))
        
        (where String_1 (δ (type v)))
        
        (side-condition (not (equal? (term String_1) 
                                     "table")))
        
        (where String_2 (errmessage StrLenWrongOp String_1))]
   
   ; Abnormal expressions with relational operators
   [--> (θ : ((v_1 == v_2)EqFail))
        (θ : (v_3 (v_1 v_2)))
        
        E-EqualityFailWithHandler
        ; Determine the type of sv_1
        (where v_3 (getEqualHandler v_1 v_2 θ))
        (side-condition (not (is_false_cond? (term v_3))))]
   
   [--> (θ : ((v_1 == v_2)EqFail))
        (θ : false)
        
        E-EqualityFailNoHandler
        ; Determine the type of sv_1
        (where v_3 (getEqualHandler v_1 v_2 θ))
        
        (side-condition (is_false_cond? (term v_3)))]
   
   [--> (θ : ((v_1 relop v_2)OrdCompWrongOps))
        (θ : (v_3 (v_1 v_2)))
        
        E-OrdCompFailWithHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey relop) 
                                  θ))
        
        (side-condition (not (is_false_cond? (term v_3))))]
   
   [--> (θ : ((v_1 < v_2)OrdCompWrongOps))
        (θ : ($builtIn error (String)))
        
        E-LessThanFailNoHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey <) 
                                  θ))
        
        (side-condition (is_false_cond? (term v_3)))
        
        (where String (errmessage OrdCompWrongOps 
                                  (δ (type v_1))
                                  (δ (type v_2))))]
   
   [--> (θ : ((v_1 <= v_2)OrdCompWrongOps))
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
   
   [--> (θ : ((v_1 <= v_2)OrdCompWrongOps))
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
                                  (δ (type v_1))
                                  (δ (type v_2))))]

   
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
   
   ; Abnormal statements involving table field assignment
   [--> (θ : (((v_1 \[ v_2 \]) = v_3)statlabel))
        (θ : ($statFunCall v_4 (v_1 v_2 v_3)))
        
        E-FieldAssignWrongKeyNormal
        ; Determine if the table efectively has a meta-table
        (where v_4 (indexMetaTable v_1 "__newindex" θ))

        (side-condition (eq? (term (δ (type v_4)))
                             (term "function")))]
   
   [--> (θ : (((v_1 \[ v_2 \]) = v_3)statlabel))
        (θ : ((v_4 \[ v_2 \]) = v_3))
        
        E-FieldAssignWrongKeyRepeat
        ; Determine if the table efectively has a meta-table
        (where v_4 (indexMetaTable v_1 "__newindex" θ))
        ; Determine if in that field we don't have a reference to a function...
        (side-condition (not (or (is_nil? (term v_4))
                                 (eq? (term (δ (type v_4)))
                                       (term "function")))))]

   ; Add new field
   [--> (θ_1 : (((objref \[ v_1 \]) = v_2)WrongKey))
        (θ_2 : \;)

        E-FieldAssignWrongKeyNoHandler
        ; Determine if the table has no meta-table or its meta-table
        ; doesn't have "__newindex" as a key
        (where any_1 (indexMetaTable objref "__newindex" θ_1))
        (side-condition (is_nil? (term any_1)))

        ; Create new field [v_1] = v_2
        (where (θ_2 objref) (δ (rawset objref v_1 v_2 θ_1)))]

   ; Something wrong: key is nil,nan
   [--> (θ_1 : (((objref \[ v_1 \]) = v_2)WrongKey))
        (θ_2 : ($err v))

        E-FieldAssignNilKey
        ; Determine if the table has no meta-table or its meta-table
        ; doesn't has "__newindex" as a key
        (where any_1 (indexMetaTable objref "__newindex" θ_1))
        (side-condition (is_nil? (term any_1)))

        (where (θ_2 ($err v)) (δ (rawset objref v_1 v_2 θ_1)))]
   
   [--> (θ : (((v_1 \[ v_2 \]) = v_3)NonTable))
        (θ : (δ (error String)))

        E-FieldAssignOverNonTableNoHandler

        ; Determine if v_1 efectively has a meta-table
        (where nil (indexMetaTable v_1 "__newindex" θ))

        (where String ,(string-append "attempt to index a "
                                      (term (δ (type v_1)))
                                      " value"))]

   ))

(provide meta)
