#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt"
         ; indexMetaTable
         "../Meta-functions/deltaBasic.rkt")


;                                                                          
;                                                                          
;                                                                          
;                ;;;                                     ;;;;     ;;;;     
;               ;                                           ;        ;     
;               ;                                           ;        ;     
;  ;       ;  ;;;;;;  ;     ;  ; ;;;;     ;;;      ;;;;     ;        ;     
;  ;       ;    ;     ;     ;  ;;   ;;   ;   ;    ;    ;    ;        ;     
;   ;  ;  ;     ;     ;     ;  ;     ;  ;              ;    ;        ;     
;   ;  ;  ;     ;     ;     ;  ;     ;  ;         ;;;;;;    ;        ;     
;   ; ; ; ;     ;     ;     ;  ;     ;  ;        ;;    ;    ;        ;     
;   ; ; ; ;     ;     ;     ;  ;     ;  ;        ;     ;    ;        ;     
;    ;   ;      ;     ;;   ;;  ;     ;   ;   ;   ;    ;;    ;        ;     
;    ;   ;      ;      ;;;; ;  ;     ;    ;;;     ;;;; ;     ;;;      ;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

; implements the meta-table mechanism for WFunCall
(define-metafunction ext-lang

  ; break loop
  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall
                                                        tid_1 ...
                                                        tid_2
                                                        tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in call"))

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__call" θ))

   (side-condition (not (is_false_cond? (term v_3))))]

  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall tid_1 ...)))
   (θ : (($statFunCall ..._1 v_3 (v_1 v_2 ...)) Meta tid_1 ... tid_2))

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__call" θ))

   (side-condition (not (is_false_cond? (term v_3))))]

  [(w_fun_call (θ : (($statFunCall ... v_1 (v_2 ...)) WFunCall tid ...)))
   (θ : (δ error String))
   
   (where String (errmessage WFunCall (δ type v_1)))]
  )

(provide w_fun_call)


;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                                ;       ;;;;                                                  
;                                ;               ;          ;                                                  
;                                ;               ;          ;                                                  
;   ; ;;;;     ;;;    ; ;;;;   ;;;;;;     ;;;;   ; ;;;      ;        ;;;               ;;;    ;;   ;;  ; ;;;   
;   ;;   ;;   ;   ;   ;;   ;;    ;       ;    ;  ;;   ;     ;       ;   ;             ;   ;    ;   ;   ;;   ;  
;   ;     ;  ;     ;  ;     ;    ;            ;  ;     ;    ;      ;     ;           ;     ;    ; ;    ;     ; 
;   ;     ;  ;     ;  ;     ;    ;       ;;;;;;  ;     ;    ;      ;     ;           ;     ;     ;     ;     ; 
;   ;     ;  ;     ;  ;     ;    ;      ;;    ;  ;     ;    ;      ;;;;;;;           ;;;;;;;     ;     ;     ; 
;   ;     ;  ;     ;  ;     ;    ;      ;     ;  ;     ;    ;      ;                 ;          ; ;    ;     ; 
;   ;     ;   ;   ;   ;     ;    ;      ;    ;;  ;;   ;     ;       ;    ;            ;    ;   ;   ;   ;;   ;  
;   ;     ;    ;;;    ;     ;     ;;;    ;;;; ;  ; ;;;       ;;;     ;;;;              ;;;;   ;;   ;;  ; ;;;   
;                                                                                                      ;       
;                                                                                                      ;       
;                                                                                                      ;       
;                                                                                                              

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable
                                      tid_1 ...
                                      tid_2
                                      tid_3 ... )))
   (θ : (δ error "loop in gettable"))

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]
  
  ; function handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable tid_1 ...)))
   (θ : ((\( (cid (v_1 v_2)) \)) Meta tid_1 ... tid_2))

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where cid (δbasic rawget tid_2 "__index" θ))]

  ; table handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable tid_1 ...)))
   (θ : ((v_3 \[ v_2 \]) Meta tid_1 ... tid_2))
   
   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable tid ...)))
   (θ : (δ error String))
   
   (where String (errmessage NonTable
                             (δ type v_1)))]
  )

(provide non_table_e)



;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                                ;                                                             
;                                                ;                                                             
;                                                ;                                                             
;  ;       ;   ; ;;;    ;;;    ; ;;;;     ;;; ;  ;    ;     ;;;    ;     ;             ;;;    ;;   ;;  ; ;;;   
;  ;       ;   ;;   ;  ;   ;   ;;   ;;   ;   ;;  ;  ;;     ;   ;    ;   ;             ;   ;    ;   ;   ;;   ;  
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ; ;      ;     ;   ;   ;            ;     ;    ; ;    ;     ; 
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ;;;      ;     ;   ;   ;            ;     ;     ;     ;     ; 
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;  ;     ;;;;;;;    ; ;             ;;;;;;;     ;     ;     ; 
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;   ;    ;          ; ;             ;          ; ;    ;     ; 
;    ;   ;     ;       ;   ;   ;     ;   ;   ;;  ;    ;    ;    ;     ;;              ;    ;   ;   ;   ;;   ;  
;    ;   ;     ;        ;;;    ;     ;    ;;; ;  ;     ;    ;;;;      ;                ;;;;   ;;   ;;  ; ;;;   
;                                             ;                       ;                                ;       
;                                        ;   ;;                       ;                                ;       
;                                         ;;;;                      ;;                                 ;       
;                                                                                                              

; implements the meta-table mechanism for WrongKey
(define-metafunction ext-lang

  ; break loop
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey
                                      tid_1 ...
                                      tid_2
                                      tid_3 ... )))
   (θ : (δ error "loop in gettable"))

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]

  ; function handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey tid_1 ... )))
   (θ : ((\( (cid (v_1 v_2)) \)) Meta tid_1 ... tid_2))

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where cid (δbasic rawget tid_2 "__index" θ))]

  ; table handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey tid_1 ...)))
   (θ : ((v_3 \[ v_2 \]) Meta tid_1 ... tid_2))
   
   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey objid ...)))
   (θ : nil)]
  )

(provide wrong_key_e)


;                                                                                   
;                                                                                   
;                                                                                   
;                        ;              ;                                           
;                        ;       ;      ;                                           
;                                ;      ;                                           
;     ;;;;     ; ;;;   ;;;     ;;;;;;   ; ;;;;  ;       ;   ;;;    ; ;;;     ;;;;;  
;    ;    ;    ;;   ;    ;       ;      ;;   ;; ;       ;  ;   ;   ;;   ;   ;     ; 
;         ;    ;         ;       ;      ;     ;  ;  ;  ;  ;     ;  ;     ;  ;       
;    ;;;;;;    ;         ;       ;      ;     ;  ;  ;  ;  ;     ;  ;     ;  ;;;;    
;   ;;    ;    ;         ;       ;      ;     ;  ; ; ; ;  ;     ;  ;     ;      ;;; 
;   ;     ;    ;         ;       ;      ;     ;  ; ; ; ;  ;     ;  ;     ;        ; 
;   ;    ;;    ;         ;       ;      ;     ;   ;   ;    ;   ;   ;;   ;   ;     ; 
;    ;;;; ;    ;      ;;;;;;;     ;;;   ;     ;   ;   ;     ;;;    ; ;;;     ;;;;;  
;                                                                  ;                
;                                                                  ;                
;                                                                  ;                
;                                                                                   

; implements the meta-table mechanism for ArithWrongOps
(define-metafunction ext-lang

  ; break loop
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps
                                          tid_1 ...
                                          tid_2
                                          tid_3 ...)))
   (θ : (δ error "loop in arithop"))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey binop)
                                     θ))]

  ; handler
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps tid_1 ...)))
   (θ : ((\( (v_3 (v_1 v_2)) \)) Meta tid_1 ... tid_2))

   ; determine if v_1 or v_2 efectively have a meta-table
   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey binop)
                                     θ))]

  ; no handler
  [(arith_wrong_ops (θ : ((v_1 binop v_2) ArithWrongOps objid ...)))
   (θ : (δ error String))
        
   (where String (errmessage ArithWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide arith_wrong_ops)


;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;              ;                                                              ;                                         
;              ;                                                              ;                                         
;    ;;;;;   ;;;;;;     ; ;;;    ;;;      ;;;    ; ;;;;     ;;;      ;;;;   ;;;;;;  ;       ;   ;;;    ; ;;;     ;;;;;  
;   ;     ;    ;        ;;   ;  ;   ;    ;   ;   ;;   ;;   ;   ;    ;    ;    ;     ;       ;  ;   ;   ;;   ;   ;     ; 
;   ;          ;        ;      ;        ;     ;  ;     ;  ;              ;    ;      ;  ;  ;  ;     ;  ;     ;  ;       
;   ;;;;       ;        ;      ;        ;     ;  ;     ;  ;         ;;;;;;    ;      ;  ;  ;  ;     ;  ;     ;  ;;;;    
;       ;;;    ;        ;      ;        ;     ;  ;     ;  ;        ;;    ;    ;      ; ; ; ;  ;     ;  ;     ;      ;;; 
;         ;    ;        ;      ;        ;     ;  ;     ;  ;        ;     ;    ;      ; ; ; ;  ;     ;  ;     ;        ; 
;   ;     ;    ;        ;       ;   ;    ;   ;   ;     ;   ;   ;   ;    ;;    ;       ;   ;    ;   ;   ;;   ;   ;     ; 
;    ;;;;;      ;;;     ;        ;;;      ;;;    ;     ;    ;;;     ;;;; ;     ;;;    ;   ;     ;;;    ; ;;;     ;;;;;  
;                                                                                                      ;                
;                                                                                                      ;                
;                                                                                                      ;                
;                                                                                                                       

; implements the meta-table mechanism for StrConcatWrongOps
(define-metafunction ext-lang

  ; break loop
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps
                                            tid_1 ...
                                            tid_2
                                            tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in concat"))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey ..)
                                     θ))]
  
  ; handler
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps tid_1 ...)))
   (θ : ((\( (v_3 (v_1 v_2)) \)) Meta tid_1 ... tid_2))
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey ..)
                                     θ))]

  ; no handler
  [(str_concat_wrong_ops (θ : ((v_1 .. v_2) StrConcatWrongOps tid ...)))
   (θ : (δ error String))
        
   (where String (errmessage StrConcatWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide str_concat_wrong_ops)


;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                                 
;   ; ;;;;     ;;;      ;;; ; ;       ;   ;;;    ; ;;;     ;;;;;  
;   ;;   ;;   ;   ;    ;   ;; ;       ;  ;   ;   ;;   ;   ;     ; 
;   ;     ;  ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;       
;   ;     ;  ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;;;;    
;   ;     ;  ;;;;;;;  ;     ;  ; ; ; ;  ;     ;  ;     ;      ;;; 
;   ;     ;  ;        ;     ;  ; ; ; ;  ;     ;  ;     ;        ; 
;   ;     ;   ;    ;   ;   ;;   ;   ;    ;   ;   ;;   ;   ;     ; 
;   ;     ;    ;;;;     ;;; ;   ;   ;     ;;;    ; ;;;     ;;;;;  
;                           ;                    ;                
;                      ;   ;;                    ;                
;                       ;;;;                     ;                
;                                                                 

; implements the meta-table mechanism for NegWrongOp
(define-metafunction ext-lang

  ; break loop
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp
                               tid_1 ...
                               tid_2
                               tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in negop"))
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey -) 
                                       θ))]
  
  ; handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp tid_1 ...)))
   (θ : ((\( (v_2 (v_1)) \)) Meta tid_1 ... tid_2))
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey -) 
                                       θ))]

  ; no handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp objid ...)))
   (θ : (δ error String))
   
   (where String (errmessage NegWrongOp (δ type v_1)))]
  )

(provide neg_wrong_op)


;                                                                                            
;                                                                                            
;                                                                                            
;                             ;;;;                                                           
;              ;                 ;                                                           
;              ;                 ;                                                           
;    ;;;;;   ;;;;;;     ; ;;;    ;        ;;;    ; ;;;;  ;       ;   ;;;    ; ;;;     ;;;;;  
;   ;     ;    ;        ;;   ;   ;       ;   ;   ;;   ;; ;       ;  ;   ;   ;;   ;   ;     ; 
;   ;          ;        ;        ;      ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;       
;   ;;;;       ;        ;        ;      ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;;;;    
;       ;;;    ;        ;        ;      ;;;;;;;  ;     ;  ; ; ; ;  ;     ;  ;     ;      ;;; 
;         ;    ;        ;        ;      ;        ;     ;  ; ; ; ;  ;     ;  ;     ;        ; 
;   ;     ;    ;        ;        ;       ;    ;  ;     ;   ;   ;    ;   ;   ;;   ;   ;     ; 
;    ;;;;;      ;;;     ;         ;;;     ;;;;   ;     ;   ;   ;     ;;;    ; ;;;     ;;;;;  
;                                                                           ;                
;                                                                           ;                
;                                                                           ;                
;                                                                                            

; implements the meta-table mechanism for StrLenWrongOp
(define-metafunction ext-lang

  ; break loop
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp
                                    tid_1 ...
                                    tid_2
                                    tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in str. len."))
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey \#) 
                                       θ))]
  
  ; handler
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp tid_1 ...)))
   (θ : ((\( (v_2 (v_1)) \)) Meta tid_1 ...  tid_2))
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey \#) 
                                       θ))]

  ; no handler: table length
  [(str_len_wrong_op (θ : ((\# tid_1) StrLenWrongOp tid_2 ...)))
   (θ : (δ \# evaluatedtable)) ; no need for context Meta
   
   (where (osp_1 ... (tid_1 (evaluatedtable any ...)) osp_2 ...)
          θ)]

  ; no handler, no tid value
  [(str_len_wrong_op (θ : ((\# v) StrLenWrongOp tid ...)))
   (θ : (δ error String_2))
        
   (where String_1 (δ type v))
        
   (where String_2 (errmessage StrLenWrongOp String_1))]
  )

(provide str_len_wrong_op)


;                                                        
;                                                        
;                                                        
;                         ;;;              ;    ;;;;     
;                        ;                 ;       ;     
;                        ;                         ;     
;     ;;;      ;;; ;   ;;;;;;    ;;;;    ;;;       ;     
;    ;   ;    ;   ;;     ;      ;    ;     ;       ;     
;   ;     ;  ;     ;     ;           ;     ;       ;     
;   ;     ;  ;     ;     ;      ;;;;;;     ;       ;     
;   ;;;;;;;  ;     ;     ;     ;;    ;     ;       ;     
;   ;        ;     ;     ;     ;     ;     ;       ;     
;    ;    ;   ;   ;;     ;     ;    ;;     ;       ;     
;     ;;;;     ;;; ;     ;      ;;;; ;  ;;;;;;;     ;;;  
;                  ;                                     
;                  ;                                     
;                  ;                                     
;                                                        

; implements the meta-table mechanism for EqFail
(define-metafunction ext-lang

  ; break loop
  [(eq_fail (θ : ((v_1 == v_2) EqFail
                               tid_1 ...
                               tid_2
                               tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in eq."))
        
   (where (tid_4 ... tid_2 tid_5 ... v_3) (getEqualHandler v_1 v_2 θ))]

  ; handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail tid_1 ...)))
   (θ : ((not (not (v_3 (v_1 v_2)))) Meta tid_1 ... tid_2 ...))

   ; we need to access to 1 or 2 meta-tables
   (where (tid_2 ... v_3) (getEqualHandler v_1 v_2 θ))]

  ; no handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail tid ...)))
   (θ : false)]
  )

(provide eq_fail)


;                                                                 
;                                                                 
;                                                                 
;                           ;                                     
;                           ;                                     
;                           ;                                     
;     ;;;      ; ;;;    ;;; ; ;       ;   ;;;    ; ;;;     ;;;;;  
;    ;   ;     ;;   ;  ;   ;; ;       ;  ;   ;   ;;   ;   ;     ; 
;   ;     ;    ;      ;     ;  ;  ;  ;  ;     ;  ;     ;  ;       
;   ;     ;    ;      ;     ;  ;  ;  ;  ;     ;  ;     ;  ;;;;    
;   ;     ;    ;      ;     ;  ; ; ; ;  ;     ;  ;     ;      ;;; 
;   ;     ;    ;      ;     ;  ; ; ; ;  ;     ;  ;     ;        ; 
;    ;   ;     ;       ;   ;;   ;   ;    ;   ;   ;;   ;   ;     ; 
;     ;;;      ;        ;;; ;   ;   ;     ;;;    ; ;;;     ;;;;;  
;                                                ;                
;                                                ;                
;                                                ;                
;                                                                 

; implements the meta-table mechanism for OrdCompWrongOps
(define-metafunction ext-lang

  ; break loop
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps
                                            tid_1 ...
                                            tid_2
                                            tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in <"))
        
   (where (tid_2 v_3) (getBinHandler v_1 
                                     v_2 
                                     (binopeventkey <) 
                                     θ))]

  ; handler
  [(ord_comp_wrong_ops_lt (θ : ((v_1 < v_2) OrdCompWrongOps tid_1 ...)))
   (θ : ((not (not (v_3 (v_1 v_2)))) Meta tid_1 ... tid_2))

   ; obtain a handler for the operation
   (where (tid_2 v_3) (getBinHandler v_1 
                                     v_2 
                                     (binopeventkey <) 
                                     θ))]

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
                                             tid_1 ...
                                             tid_2
                                             tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in <="))
        
   (where (tid_2 v_3) (getBinHandler v_1 
                                     v_2 
                                     (binopeventkey <=) 
                                     θ))]
  
  ; handler
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps tid_1 ...)))
   (θ : ((not (not (v_3 (v_1 v_2)))) Meta tid_1 ... tid_2))

   ; obtain a handler for the operation
   (where (tid_2 v_3) (getBinHandler v_1 
                                     v_2 
                                     (binopeventkey <=) 
                                     θ))]

  ; try with not (v_2 < v_1)
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps tid_1 ...)))
   (θ : ((not (v_3 (v_2 v_1))) Meta tid_1 ... tid_2))

   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey <)
                                     θ))]

  ; no handler
  [(ord_comp_wrong_ops_le (θ : ((v_1 <= v_2) OrdCompWrongOps tid ...)))
   (θ : (δ error String))
        
   (where String (errmessage OrdCompWrongOps
                             (δ type v_1)
                             (δ type v_2)))]
  )

(provide ord_comp_wrong_ops_le)


;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;                                                ;       ;;;;                                                           
;                                ;               ;          ;                                   ;                 ;     
;                                ;               ;          ;                                   ;                 ;     
;   ; ;;;;     ;;;    ; ;;;;   ;;;;;;     ;;;;   ; ;;;      ;        ;;;              ;;;;;   ;;;;;;     ;;;;   ;;;;;;  
;   ;;   ;;   ;   ;   ;;   ;;    ;       ;    ;  ;;   ;     ;       ;   ;            ;     ;    ;       ;    ;    ;     
;   ;     ;  ;     ;  ;     ;    ;            ;  ;     ;    ;      ;     ;           ;          ;            ;    ;     
;   ;     ;  ;     ;  ;     ;    ;       ;;;;;;  ;     ;    ;      ;     ;           ;;;;       ;       ;;;;;;    ;     
;   ;     ;  ;     ;  ;     ;    ;      ;;    ;  ;     ;    ;      ;;;;;;;               ;;;    ;      ;;    ;    ;     
;   ;     ;  ;     ;  ;     ;    ;      ;     ;  ;     ;    ;      ;                       ;    ;      ;     ;    ;     
;   ;     ;   ;   ;   ;     ;    ;      ;    ;;  ;;   ;     ;       ;    ;           ;     ;    ;      ;    ;;    ;     
;   ;     ;    ;;;    ;     ;     ;;;    ;;;; ;  ; ;;;       ;;;     ;;;;             ;;;;;      ;;;    ;;;; ;     ;;;  
;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;                                                                                                                       

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable
                                              tid_1 ...
                                              tid_2
                                              tid_3 ...)))
   ; TODO: check this
   (θ : (δ error "loop in settable"))
   
    ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_4 (δbasic rawget tid_2 "__newindex" θ))

    (side-condition (not (is_nil? (term v_4))))]
  
  ; function handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable tid_1 ...)))
   (θ : (($statFunCall cid (v_1 v_2 v_3)) Meta tid_1 ... tid_2))

   ; determine if v_1 has a meta-table with a handler
   (where tid_2 (getMetaTable v_1 θ))
   (where cid (δbasic rawget tid_2 "__newindex" θ))]

  ; table handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable tid_1 ...)))
   (θ : (((v_4 \[ v_2 \]) = v_3) Meta tid_1 ... tid_2))
        
   ; determine if v_1 has a meta-table with a handler
   (where tid_2 (getMetaTable v_1 θ))
   (where v_4 (δbasic rawget tid_2 "__newindex" θ))

   (side-condition (not (is_nil? (term v_4))))]

  ; no handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable tid ...)))
   (θ : (δ error String))

   (where String ,(string-append "attempt to index a "
                                 (term (δ type v_1))
                                 " value"))]
  )

(provide non_table_s)


;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;                                                ;                                                                      
;                                                ;                                              ;                 ;     
;                                                ;                                              ;                 ;     
;  ;       ;   ; ;;;    ;;;    ; ;;;;     ;;; ;  ;    ;     ;;;    ;     ;            ;;;;;   ;;;;;;     ;;;;   ;;;;;;  
;  ;       ;   ;;   ;  ;   ;   ;;   ;;   ;   ;;  ;  ;;     ;   ;    ;   ;            ;     ;    ;       ;    ;    ;     
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ; ;      ;     ;   ;   ;            ;          ;            ;    ;     
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ;;;      ;     ;   ;   ;            ;;;;       ;       ;;;;;;    ;     
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;  ;     ;;;;;;;    ; ;                 ;;;    ;      ;;    ;    ;     
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;   ;    ;          ; ;                   ;    ;      ;     ;    ;     
;    ;   ;     ;       ;   ;   ;     ;   ;   ;;  ;    ;    ;    ;     ;;             ;     ;    ;      ;    ;;    ;     
;    ;   ;     ;        ;;;    ;     ;    ;;; ;  ;     ;    ;;;;      ;               ;;;;;      ;;;    ;;;; ;     ;;;  
;                                             ;                       ;                                                 
;                                        ;   ;;                       ;                                                 
;                                         ;;;;                      ;;                                                  
;                                                                                                                       

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(wrong_key_s (θ : (((tid_1 \[ v_1 \]) = v_2) WrongKey
                                              tid_2 ...
                                              tid_3
                                              tid_4 ...)))
   ; TODO: check this
   (θ : (δ error "loop in settable"))

   ; determine if v_1 has a meta-table with a handler
   (where tid_3 (getMetaTable tid_1 θ))
   (where v_3 (δbasic rawget tid_3 "__newindex" θ))

   (side-condition (not (is_nil? (term v_3))))]
  
  ; function handler
  [(wrong_key_s (θ : (((tid_1 \[ v_1 \]) = v_2) WrongKey tid_2 ...)))
   (θ : (($statFunCall cid (tid_1 v_1 v_2)) Meta tid_2 ... tid_3))

   ; determine if v_1 has a meta-table with a handler
   (where tid_3 (getMetaTable tid_1 θ))
   (where cid (δbasic rawget tid_3 "__newindex" θ))]

  ; table handler
  [(wrong_key_s (θ : (((tid_1 \[ v_1 \]) = v_2) WrongKey tid_2 ...)))
   (θ : (((v_3 \[ v_1 \]) = v_2) Meta tid_2 ... tid_3))
        
   ; determine if v_1 has a meta-table with a handler
   (where tid_3 (getMetaTable tid_1 θ))
   (where v_3 (δbasic rawget tid_3 "__newindex" θ))
   
   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(wrong_key_s (θ_1 : (((tid_1 \[ v_1 \]) = v_2) WrongKey tid_2 ...)))
   (θ_2 : any_2)

   ; try to create new field [v_1] = v_2
   (where (θ_2 any_1) (δ rawset tid_1 v_1 v_2 θ_1))

   ; any_2 should be either skip (rawset was successful) or $err v
   ; in this way we simplify rules
   (where any_2 ,(if (is_tid? (term any_1))
                     (term \;) ; success
                     (term any_1) ; any_1 is an error object: key was
                     ; nil,nan
                     ))]
  )

(provide wrong_key_s)
