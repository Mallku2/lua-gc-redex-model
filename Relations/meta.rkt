#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/metaTablesMetaFunctions.rkt")

; expressions handled by the meta-table mechanism.

(define meta
  (reduction-relation
   ext-lang
   #:arrow -->meta
   ;#:domain (θ : t)

   ; WFunCall
   [-->meta (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall objid ...))
            (θ : t)
        
            E-WFunCall
            
            (where t
                   (w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...))
                                     WFunCall
                                     objid ...))))]

   ; NonTable exp.
   [-->meta (θ : ((v_1 \[ v_2 \]) NonTable objid ...))
            (θ : e)
        
            E-NonTableExp
            
            (where e
                   (non_table_e (θ : ((v_1 \[ v_2 \]) NonTable objid ...))))]

   ; WrongKey exp.
   [-->meta (θ : ((v_1 \[ v_2 \]) WrongKey objid ... ))
            (θ : e)
        
            E-WrongKeyExp
            
            (where e
                   (wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey objid ...))))]

   ; ArithWrongOps
   [-->meta (θ : ((v_1 binop v_2) ArithWrongOps objid ...))
            (θ : e)
        
            E-ArithWrongOps
            
            (where e
                   (arith_wrong_ops (θ : ((v_1 binop v_2)
                                            ArithWrongOps
                                            objid ...))))]

   ; StrConcatWrongOps
   [-->meta (θ : ((v_1 .. v_2) StrConcatWrongOps objid ...))
            (θ : e)
        
            E-StrConcatWrongOps
            
            (where e
                   (str_concat_wrong_ops (θ : ((v_1 .. v_2)
                                                 StrConcatWrongOps
                                                 objid ...))))] 

   ; NegWrongOp
   [-->meta (θ : ((- v) NegWrongOp objid ...))
            (θ : e)
        
            E-NegWrongOp
            
            (where e
                   (neg_wrong_op (θ : ((- v) NegWrongOp objid ...))))]

   ; StrLenWrongOp
   [-->meta (θ : ((\# v) StrLenWrongOp objid ...))
            (θ : e)
        
            E-StrLenWrongOp
            
            (where e
                   (str_len_wrong_op (θ : ((\# v) StrLenWrongOp objid ...))))]
   
   
   ; abnormal expressions with relational operators
   ; EqFail
   [-->meta (θ : ((v_1 == v_2) EqFail objid ...))
            (θ : e)
        
            E-EqFail
            
            (where e
                   (eq_fail (θ : ((v_1 == v_2) EqFail objid ...))))]

   ; OrdCompWrongOps
   [-->meta (θ : ((v_1 < v_2) OrdCompWrongOps objid ...))
            (θ : e)
        
            E-OrdCompWrongOpsLt
            
            (where e
                   (ord_comp_wrong_ops_lt (θ : ((v_1 < v_2)
                                                  OrdCompWrongOps
                                                  objid ...))))]

   [-->meta (θ : ((v_1 <= v_2) OrdCompWrongOps objid ...))
            (θ : e)
        
            E-OrdCompWrongOpsLe
            
            (where e
                   (ord_comp_wrong_ops_le (θ : ((v_1 <= v_2)
                                                  OrdCompWrongOps
                                                  objid ...))))] 


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
   [-->meta (θ : (((v_1 \[ v_2 \]) = v_3) NonTable objid ...))
            (θ : s)
        
            E-NonTableStat
            
            (where s
                   (non_table_s (θ : (((v_1 \[ v_2 \]) = v_3)
                                        NonTable
                                        objid ...))))]

   [-->meta (θ_1 : (((v_1 \[ v_2 \]) = v_3) WrongKey objid ...))
            (θ_2 : s)
        
            E-WrongKeyStat
            
            (where (θ_2 : s)
                   (wrong_key_s (θ_1 : (((v_1 \[ v_2 \]) = v_3)
                                        WrongKey
                                        objid ...))))] 

   ))

(provide meta)
