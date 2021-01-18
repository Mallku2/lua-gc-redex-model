#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         ; indexMetaTable
         "../Meta-functions/deltaBasic.rkt")

; implements the meta-table mechanism for WFunCall
(define-metafunction ext-lang

  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall)))
   (θ : ($statFunCall ..._1 any (v_1 v_2 ...)))

   ; determine if v_1 has a meta-table
   (where any (indexMetaTable v_1 
                              "__call" 
                              θ))
   
   (side-condition (not (is_false_cond? (term any))))]

  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall)))
   (θ : (δ error String))

   (where v_3 (indexMetaTable v_1 
                              "__call" 
                              θ))
   
   (side-condition (is_false_cond? (term v_3)))
   
   (where String (errmessage WFunCall (δ type v_1)))]
  )

(provide w_fun_call)

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; function handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable)))
   (θ : (cid (v_1 v_2)))

   ; determine if v_1 has a meta-table
   (where cid (indexMetaTable v_1 "__index" θ))]

  ; table handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable)))
   (θ : (v_3 \[ v_2 \]))
   
   (where v_3 (indexMetaTable v_1 "__index" θ))
   
   (side-condition (not (or (is_nil? (term v_3))
                            (eq? (term (δ type v_3))
                                 "function"))))]

  ; no handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable)))
   (θ : ($builtIn error (String)))

   ; determine if simplevalue efectively has a meta-table
   (where nil (indexMetaTable v_1 
                              "__index" 
                              θ))
   
   (where String (errmessage NonTable
                             (δ type v_1)))]
  )

(provide non_table_e)


; implements the meta-table mechanism for WrongKey
(define-metafunction ext-lang

  ; break loop
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) objid_1 ... objid_2 objid_3 ... WrongKey)))
   (θ : (δ error "loop in gettable"))

   ; meta-method already invoked 
   (where objid_2 (indexMetaTable v_1 "__index" θ))]

  ; function handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) objid ... WrongKey)))
   (θ : ((cid (v_1 v_2)) WrongKey objid ... cid))

   ; determine if v_1 has a meta-table
   (where cid (indexMetaTable v_1 "__index" θ))]

  ; table handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) objid ... WrongKey)))
   (θ : ((tid \[ v_2 \]) WrongKey objid ... tid))
   
   (where tid (indexMetaTable v_1 "__index" θ))]

  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) objid ... WrongKey)))
   (θ : ((v_3 \[ v_2 \]) WrongKey objid ...))
   
   (where v_3 (indexMetaTable v_1 "__index" θ))

   ; {v_3 ∉ tid}
   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) objid ... WrongKey)))
   (θ : nil)

   ; determine if simplevalue efectively has a meta-table
   (where nil (indexMetaTable v_1 
                              "__index" 
                              θ))]
  )

(provide wrong_key_e)

; implements the meta-table mechanism for ArithWrongOps
(define-metafunction ext-lang

  ; handler
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps)))
   (θ : (v_3 (v_1 v_2)))

   (side-condition (is_arithop? (term binop)))
        
   ; determine if v_1 or v_2 efectively has a meta-table
   (where v_3 (getBinHandler v_1
                             v_2
                             (binopeventkey binop)
                             θ))
    
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps)))
   (θ : ($builtIn error (String)))

   (side-condition (is_arithop? (term binop)))
        
   ; determine if v_1 or v_2 efectively has a meta-table
   (where v_3 (getBinHandler v_1 v_2 (binopeventkey binop) θ))

   (side-condition (is_false_cond? (term v_3)))
        
   (where String (errmessage ArithWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide arith_wrong_ops)

; implements the meta-table mechanism for StrConcatWrongOps
(define-metafunction ext-lang

  ; handler
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps)))
   (θ : (v_3 (v_1 v_2)))
        
   ; determine if v_1 or v_2 efectively has a meta-table
   (where v_3 (getBinHandler v_1
                             v_2
                             (binopeventkey ..)
                             θ))
    
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps)))
   (θ : ($builtIn error (String)))
        
   ; determine if v_1 or v_2 efectively has a meta-table
   (where v_3 (getBinHandler v_1 v_2 (binopeventkey ..) θ))

   (side-condition (is_false_cond? (term v_3)))
        
   (where String (errmessage StrConcatWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide str_concat_wrong_ops)

; implements the meta-table mechanism for NegWrongOp
(define-metafunction ext-lang

  ; handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp)))
   (θ : (v_2 (v_1)))
        
   ; determine if v_1 efectively has a meta-table
   (where v_2 (getUnaryHandler v_1 
                               (unopeventkey -) 
                               θ))
   
   (side-condition (not (is_false_cond? (term v_2))))]

  ; no handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp)))
   (θ : ($builtIn error (String)))
        
   ; determine if simplevalue efectively has a meta-table
   (where v_2 (getUnaryHandler v_1
                               (unopeventkey -)
                               θ))
   
   (side-condition (is_false_cond? (term v_2)))
   
   (where String (errmessage NegWrongOp (δ type v_1)))]
  )

(provide neg_wrong_op)

; implements the meta-table mechanism for StrLenWrongOp
(define-metafunction ext-lang

  ; handler
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp)))
   (θ : (v_2 (v_1)))
        
   ; determine if we have a handler for the operation
   (where v_2 (getUnaryHandler v_1 
                               (unopeventkey \#) 
                               θ))
        
   (side-condition (not (is_false_cond? (term v_2))))]

  ; table length
  [(str_len_wrong_op (θ : ((\# objref) StrLenWrongOp)))
   (θ : (δ \# evaluatedtable))
   
   ; determine if we have a handler for the operation
   (where nil (getUnaryHandler objref
                               (unopeventkey \#)
                               θ))
   
   (where (osp_1 ... (objref (evaluatedtable any ...)) osp_2 ...)
          θ)]

  [(str_len_wrong_op (θ : ((\# v) StrLenWrongOp)))
   (θ : ($builtIn error (String_2)))
   
   ; determine if we have a handler for the operation
   (where nil (getUnaryHandler v 
                               (unopeventkey \#)
                               θ))
        
   (where String_1 (δ type v))
        
   (side-condition (not (equal? (term String_1) 
                                "table")))
        
   (where String_2 (errmessage StrLenWrongOp String_1))]
  )

(provide str_len_wrong_op)

; implements the meta-table mechanism for EqFail
(define-metafunction ext-lang

  ; handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail)))
   (θ : (v_3 (v_1 v_2)))

   (where v_3 (getEqualHandler v_1 v_2 θ))
   
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail)))
   (θ : false)

   (where v_3 (getEqualHandler v_1 v_2 θ))
   
   (side-condition (is_false_cond? (term v_3)))]
  )

(provide eq_fail)

; implements the meta-table mechanism for OrdCompWrongOps
(define-metafunction ext-lang

  ; handler
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps)))
   (θ : (v_3 (v_1 v_2)))

   ; obtain a handler for the operation
   (where v_3 (getBinHandler v_1 
                             v_2 
                             (binopeventkey <) 
                             θ))
   
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps)))
   (θ : ($builtIn error (String)))

   ; obtain a handler for the operation
   (where v_3 (getBinHandler v_1 
                             v_2 
                             (binopeventkey <) 
                             θ))
        
   (side-condition (is_false_cond? (term v_3)))
        
   (where String (errmessage OrdCompWrongOps 
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide ord_comp_wrong_ops_lt)

(define-metafunction ext-lang

  ; handler
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps)))
   (θ : (v_3 (v_1 v_2)))

   ; obtain a handler for the operation
   (where v_3 (getBinHandler v_1 
                             v_2 
                             (binopeventkey <=) 
                             θ))
   
   (side-condition (not (is_false_cond? (term v_3))))]

  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps)))
   (θ : (not (v_4 (v_2 v_1))))
        
   ; obtain a handler for the operation
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

  ; no handler
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps)))
   (θ : ($builtIn error (String)))
        
   ; determine if v_1,v_2 efectively have a meta-table
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
  )

(provide ord_comp_wrong_ops_le)


; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; function handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable)))
   (θ : ($statFunCall cid (v_1 v_2 v_3)))

   ; determine if v_1 has a meta-table with a handler
   (where cid (indexMetaTable v_1 "__newindex" θ))]

  ; table handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable)))
   (θ : ((v_4 \[ v_2 \]) = v_3))
        
   ; determine if v_1 has a meta-table with a handler
   (where v_4 (indexMetaTable v_1 "__newindex" θ))
   ; determine if in that field we don't have a reference to a function...
   (side-condition (not (or (is_nil? (term v_4))
                            (eq? (term (δ type v_4))
                                 (term "function")))))]
   

  ; no handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable)))
   (θ : (δ error String))

   ; determine if v_1 efectively has a meta-table
   (where nil (indexMetaTable v_1 "__newindex" θ))

   (where String ,(string-append "attempt to index a "
                                 (term (δ type v_1))
                                 " value"))]
  )

(provide non_table_s)

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; function handler
  [(wrong_key_s (θ : (((tid \[ v_2 \]) = v_3) WrongKey)))
   (θ : ($statFunCall cid (tid v_2 v_3)))

   ; determine if v_1 has a meta-table with a handler
   (where cid (indexMetaTable tid "__newindex" θ))]

  ; table handler
  [(wrong_key_s (θ : (((tid \[ v_2 \]) = v_3) WrongKey)))
   (θ : ((v_4 \[ v_2 \]) = v_3))
        
   ; determine if v_1 has a meta-table with a handler
   (where v_4 (indexMetaTable tid "__newindex" θ))
   ; determine if in that field we don't have a reference to a function...
   (side-condition (not (or (is_nil? (term v_4))
                            (eq? (term (δ type v_4))
                                 (term "function")))))]
   

  ; no handler
  [(wrong_key_s (θ_1 : (((tid \[ v_2 \]) = v_3) WrongKey)))
   (θ_2 : any_2)

   ; determine if the table has no meta-table or its meta-table
   ; doesn't have "__newindex" as a key
   (where nil (indexMetaTable tid "__newindex" θ_1))

   ; try to create new field [v_1] = v_2
   (where (θ_2 any_1) (δ rawset tid v_2 v_3 θ_1))

   ; any_2 should be either skip (rawset was successful) or $err v
   ; in this way we simplify rules
   (where any_2 ,(if (is_tid? (term any_1))
                     (term \;) ; success
                     (term any_1) ; any_1 is an error object: key was
                                  ; nil,nan
                     ))]
  )

(provide wrong_key_s)
