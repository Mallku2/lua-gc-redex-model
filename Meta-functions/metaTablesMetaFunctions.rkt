#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         ; indexMetaTable
         "../Meta-functions/deltaBasic.rkt")

; implements the meta-table mechanism for WFunCall
(define-metafunction ext-lang

  ; break loop
  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall
                                                        objid_1 ...
                                                        objid_2
                                                        objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in call"))

   ; determine if v_1 has a meta-table
   (where objid_2 (indexMetaTable v_1 
                                  "__call"
                                  θ))]

  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall objid ...)))
   (θ : (($statFunCall ..._1 tid (v_1 v_2 ...)) Meta objid ... tid))

   ; determine if v_1 has a meta-table
   (where tid (indexMetaTable v_1 
                                  "__call" 
                                  θ))]

  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall objid ...)))
   (θ : (($statFunCall ..._1 v_3 (v_1 v_2 ...)) Meta objid ...))

   ; determine if v_1 has a meta-table
   (where v_3 (indexMetaTable v_1 
                              "__call" 
                              θ))
   
   (side-condition (not (is_false_cond? (term v_3))))]

  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall objid ...)))
   (θ : (δ error String))
   
   (where String (errmessage WFunCall (δ type v_1)))]
  )

(provide w_fun_call)

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable
                                      objid_1 ...
                                      objid_2
                                      objid_3 ... )))
   (θ : (δ error "loop in gettable"))

   ; meta-method already invoked 
   (where objid_2 (indexMetaTable v_1 "__index" θ))]
  
  ; function handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable objid ...)))
   (θ : ((\( (cid (v_1 v_2)) \)) Meta objid ...))

   ; determine if v_1 has a meta-table
   (where cid (indexMetaTable v_1 "__index" θ))]

  ; table handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable objid ...)))
   (θ : ((tid \[ v_2 \]) Meta objid ... tid))
   
   (where tid (indexMetaTable v_1 "__index" θ))]
  
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable objid ...)))
   (θ : ((v_3 \[ v_2 \]) Meta objid ...))
   
   (where v_3 (indexMetaTable v_1 "__index" θ))
   
   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable objid ...)))
   (θ : (δ error String))
   
   (where String (errmessage NonTable
                             (δ type v_1)))]
  )

(provide non_table_e)


; implements the meta-table mechanism for WrongKey
(define-metafunction ext-lang

  ; break loop
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey
                                      objid_1 ...
                                      objid_2
                                      objid_3 ... )))
   (θ : (δ error "loop in gettable"))

   ; meta-method already invoked 
   (where objid_2 (indexMetaTable v_1 "__index" θ))]

  ; function handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey objid ... )))
   ; TODO: check if cid is taken into account
   (θ : ((\( (cid (v_1 v_2)) \)) Meta objid ...))

   ; determine if v_1 has a meta-table
   (where cid (indexMetaTable v_1 "__index" θ))]

  ; table handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey objid ...)))
   (θ : ((tid \[ v_2 \]) Meta objid ... tid))
   
   (where tid (indexMetaTable v_1 "__index" θ))]

  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey objid ... )))
   (θ : ((v_3 \[ v_2 \]) Meta objid ...))
   
   (where v_3 (indexMetaTable v_1 "__index" θ))

   ; {v_3 ∉ tid}
   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey objid ...)))
   (θ : nil)]
  )

(provide wrong_key_e)

; implements the meta-table mechanism for ArithWrongOps
(define-metafunction ext-lang

  ; break loop
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps
                                          objid_1 ...
                                          objid_2
                                          objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in arithop"))

   (side-condition (is_arithop? (term binop)))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where objid_2 (getBinHandler v_1
                                 v_2
                                 (binopeventkey binop)
                                 θ))]

  ; handler
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps objid ...)))
   (θ : ((\( (tid (v_1 v_2)) \)) Meta objid ... tid))

   (side-condition (is_arithop? (term binop)))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where tid (getBinHandler v_1
                                 v_2
                                 (binopeventkey binop)
                                 θ))]

  
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps objid ...)))
   (θ : ((\( (v_3 (v_1 v_2)) \)) Meta objid ...))

   (side-condition (is_arithop? (term binop)))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where v_3 (getBinHandler v_1
                             v_2
                             (binopeventkey binop)
                             θ))
    
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps objid ...)))
   (θ : (δ error String))
        
   (where String (errmessage ArithWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide arith_wrong_ops)

; implements the meta-table mechanism for StrConcatWrongOps
(define-metafunction ext-lang

  ; break loop
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps
                                            objid_1 ...
                                            objid_2
                                            objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in concat"))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where objid_2 (getBinHandler v_1
                                 v_2
                                 (binopeventkey ..)
                                 θ))]
  
  ; handler
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps objid ...)))
   (θ : ((\( (tid (v_1 v_2)) \)) Meta objid ... tid))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where tid (getBinHandler v_1
                             v_2
                             (binopeventkey ..)
                             θ))]
  
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps objid ...)))
   (θ : ((\( (v_3 (v_1 v_2)) \)) Meta objid ...))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where v_3 (getBinHandler v_1
                             v_2
                             (binopeventkey ..)
                             θ))
    
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps objid ...)))
   (θ : (δ error String))
        
   (where String (errmessage StrConcatWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide str_concat_wrong_ops)

; implements the meta-table mechanism for NegWrongOp
(define-metafunction ext-lang

  ; break loop
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp
                               objid_1 ...
                               objid_2
                               objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in negop"))
        
   ; determine if v_1 efectively has a meta-table
   (where objid_2 (getUnaryHandler v_1 
                                   (unopeventkey -) 
                                   θ))]
  
  ; handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp objid ...)))
   (θ : ((\( (tid (v_1)) \)) Meta objid ... tid))
        
   ; determine if v_1 efectively has a meta-table
   (where tid (getUnaryHandler v_1 
                               (unopeventkey -) 
                               θ))]
  
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp objid ...)))
   (θ : ((\( (v_2 (v_1)) \)) Meta objid ...))
        
   ; determine if v_1 efectively has a meta-table
   (where v_2 (getUnaryHandler v_1 
                               (unopeventkey -) 
                               θ))
   
   (side-condition (not (is_false_cond? (term v_2))))]

  ; no handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp objid ...)))
   (θ : (δ error String))
   
   (where String (errmessage NegWrongOp (δ type v_1)))]
  )

(provide neg_wrong_op)

; implements the meta-table mechanism for StrLenWrongOp
(define-metafunction ext-lang

  ; break loop
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp
                                    objid_1 ...
                                    objid_2
                                    objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in str. len."))
        
   ; determine if v_1 efectively has a meta-table
   (where objid_2 (getUnaryHandler v_1 
                                   (unopeventkey \#) 
                                   θ))]
  
  ; handler
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp objid ...)))
   (θ : ((\( (tid (v_1)) \)) Meta objid ...  tid))
        
   ; determine if v_1 efectively has a meta-table
   (where tid (getUnaryHandler v_1 
                               (unopeventkey \#) 
                               θ))]
  
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp objid ...)))
   (θ : ((\( (v_2 (v_1)) \)) Meta objid ...))
        
   ; determine if we have a handler for the operation
   (where v_2 (getUnaryHandler v_1 
                               (unopeventkey \#) 
                               θ))
        
   (side-condition (not (is_false_cond? (term v_2))))]

  ; table length
  [(str_len_wrong_op (θ : ((\# tid) StrLenWrongOp objid ...)))
   (θ : (δ \# evaluatedtable)) ; no need for context Meta
   
   ; determine if we have a handler for the operation
   (where nil (getUnaryHandler tid
                               (unopeventkey \#)
                               θ))
   
   (where (osp_1 ... (tid (evaluatedtable any ...)) osp_2 ...)
          θ)]

  [(str_len_wrong_op (θ : ((\# v) StrLenWrongOp objid ...)))
   (θ : (δ error String_2))
        
   (where String_1 (δ type v))
        
   (where String_2 (errmessage StrLenWrongOp String_1))]
  )

(provide str_len_wrong_op)

; implements the meta-table mechanism for EqFail
(define-metafunction ext-lang

  ; break loop
  [(eq_fail (θ : ((v_1 == v_2) EqFail
                               objid_1 ...
                               objid_2
                               objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in eq."))
        
   (where objid_2 (getEqualHandler v_1 v_2 θ))]

  ; handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail objid ...)))
   (θ : ((not (not (tid (v_1 v_2)))) Meta objid ... tid))

   (where tid (getEqualHandler v_1 v_2 θ))]

  [(eq_fail (θ : ((v_1 == v_2) EqFail objid ...)))
   (θ : ((not (not (v_3 (v_1 v_2)))) Meta objid ...))

   (where v_3 (getEqualHandler v_1 v_2 θ))
   
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail objid ...)))
   (θ : false)]
  )

(provide eq_fail)

; implements the meta-table mechanism for OrdCompWrongOps
(define-metafunction ext-lang

  ; break loop
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps
                                            objid_1 ...
                                            objid_2
                                            objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in <"))
        
   (where objid_2 (getBinHandler v_1 
                                 v_2 
                                 (binopeventkey <) 
                                 θ))]

  ; handler
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps objid ...)))
   (θ : ((not (not (tid (v_1 v_2)))) Meta objid ... tid))

   ; obtain a handler for the operation
   (where tid (getBinHandler v_1 
                             v_2 
                             (binopeventkey <) 
                             θ))]
  
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps objid ...)))
   (θ : ((not (not (v_3 (v_1 v_2)))) Meta objid ...))

   ; obtain a handler for the operation
   (where v_3 (getBinHandler v_1 
                             v_2 
                             (binopeventkey <) 
                             θ))
   
   (side-condition (not (is_false_cond? (term v_3))))]

  ; no handler
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps objid ...)))
   (θ : (δ error String))
        
   (where String (errmessage OrdCompWrongOps 
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide ord_comp_wrong_ops_lt)

(define-metafunction ext-lang

  ; break loop
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps
                                             objid_1 ...
                                             objid_2
                                             objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in <="))
        
   (where objid_2 (getBinHandler v_1 
                                 v_2 
                                 (binopeventkey <=) 
                                 θ))]
  
  ; handler
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps objid ...)))
   (θ : ((not (not (tid (v_1 v_2)))) Meta objid ... tid))

   ; obtain a handler for the operation
   (where tid (getBinHandler v_1 
                                 v_2 
                                 (binopeventkey <=) 
                                 θ))]

  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps objid ...)))
   (θ : ((not (not (v_3 (v_1 v_2)))) Meta objid ...))

   ; obtain a handler for the operation
   (where v_3 (getBinHandler v_1 
                             v_2 
                             (binopeventkey <=) 
                             θ))

   (side-condition (not (is_false_cond? (term v_3))))]

  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps objid ...)))
   (θ : ((not (tid (v_2 v_1))) Meta objid ... tid))
        
   ; obtain a handler for the operation
   (where v_3 (getBinHandler v_1 
                             v_2 
                             (binopeventkey <=) 
                             θ))
        
   (side-condition (is_false_cond? (term v_3)))

   (where tid (getBinHandler v_1
                             v_2
                             (binopeventkey <)
                             θ))]

  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps objid ...)))
   (θ : ((not (v_4 (v_2 v_1))) Meta objid ...))
        
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
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps objid ...)))
   (θ : (δ error String))
        
   (where String (errmessage OrdCompWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide ord_comp_wrong_ops_le)


; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable
                                              objid_1 ...
                                              objid_2
                                              objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in settable"))
        
   (where objid_2 (indexMetaTable v_1 "__newindex" θ))]
  
  ; function handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable objid ...)))
   (θ : (($statFunCall cid (v_1 v_2 v_3)) Meta objid ...))

   ; determine if v_1 has a meta-table with a handler
   (where cid (indexMetaTable v_1 "__newindex" θ))]

  ; table handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable objid ...)))
   (θ : (((tid \[ v_2 \]) = v_3) Meta objid ... tid))
        
   ; determine if v_1 has a meta-table with a handler
   (where tid (indexMetaTable v_1 "__newindex" θ))]
  
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable objid ...)))
   (θ : (((v_4 \[ v_2 \]) = v_3) Meta objid ...))
        
   ; determine if v_1 has a meta-table with a handler
   (where v_4 (indexMetaTable v_1 "__newindex" θ))
   ; determine if in that field we don't have a reference to a function...
   (side-condition (not (is_nil? (term v_4))))]

  ; no handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable objid ...)))
   (θ : (δ error String))

   (where String ,(string-append "attempt to index a "
                                 (term (δ type v_1))
                                 " value"))]
  )

(provide non_table_s)

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(wrong_key_s (θ : (((v_1 \[ v_2 \]) = v_3) WrongKey
                                              objid_1 ...
                                              objid_2
                                              objid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in settable"))
        
   (where objid_2 (indexMetaTable v_1 "__newindex" θ))]
  
  ; function handler
  [(wrong_key_s (θ : (((tid \[ v_2 \]) = v_3) WrongKey objid ...)))
   (θ : (($statFunCall cid (tid v_2 v_3)) Meta objid ...))

   ; determine if v_1 has a meta-table with a handler
   (where cid (indexMetaTable tid "__newindex" θ))]

  ; table handler
  [(wrong_key_s (θ : (((tid_1 \[ v_2 \]) = v_3) WrongKey objid ...)))
   (θ : (((tid_2 \[ v_2 \]) = v_3) Meta objid ... tid_2))
        
   ; determine if v_1 has a meta-table with a handler
   (where tid_2 (indexMetaTable tid_1 "__newindex" θ))]

  [(wrong_key_s (θ : (((tid \[ v_2 \]) = v_3) WrongKey objid ...)))
   (θ : (((v_4 \[ v_2 \]) = v_3) Meta objid ...))
        
   ; determine if v_1 has a meta-table with a handler
   (where v_4 (indexMetaTable tid "__newindex" θ))
   
   (side-condition (not (is_nil? (term v_4))))]

  ; no handler
  [(wrong_key_s (θ_1 : (((tid \[ v_2 \]) = v_3) WrongKey objid ...)))
   (θ_2 : any_2)

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
