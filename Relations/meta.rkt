#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetaFunctions.rkt"
         "../Meta-functions/metaTablesMetaFunctions.rkt"
         "../Meta-functions/delta.rkt"
         ; indexMetaTable
         "../Meta-functions/deltaBasic.rkt")

; expressions handled by the meta-table mechanism.

(define meta
  (reduction-relation
   ext-lang
   #:arrow -->meta
   ;#:domain (θ : t)

   ; WFunCall
   [-->meta (θ_1 : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall))
            (θ_2 : t)
        
            E-WFunCall
            
            (where (θ_2 : t)
                   (w_fun_call (θ_1 : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall))))]

   ; NonTable exp.
   [-->meta (θ_1 : ((v_1 \[ v_2 \]) NonTable))
            (θ_2 : e)
        
            E-NonTableExp
            
            (where (θ_2 : e)
                   (non_table_e (θ_1 : ((v_1 \[ v_2 \]) NonTable))))]

   ; WrongKey exp.
   [-->meta (θ_1 : ((v_1 \[ v_2 \]) WrongKey))
            (θ_2 : e)
        
            E-WrongKeyExp
            
            (where (θ_2 : e)
                   (wrong_key_e (θ_1 : ((v_1 \[ v_2 \]) WrongKey))))]

   ; ArithWrongOps
   [-->meta (θ_1 : ((v_1 binop v_2) ArithWrongOps))
            (θ_2 : e)
        
            E-ArithWrongOps
            
            (where (θ_2 : e)
                   (arith_wrong_ops (θ_1 : ((v_1 binop v_2) ArithWrongOps))))]

   ; StrConcatWrongOps
   [-->meta (θ_1 : ((v_1 .. v_2) StrConcatWrongOps))
            (θ_2 : e)
        
            E-StrConcatWrongOps
            
            (where (θ_2 : e)
                   (str_concat_wrong_ops (θ_1 : ((v_1 .. v_2) StrConcatWrongOps))))] 

   ; NegWrongOp
   [-->meta (θ_1 : ((- v) NegWrongOp))
            (θ_2 : e)
        
            E-NegWrongOp
            
            (where (θ_2 : e)
                   (neg_wrong_op (θ_1 : ((- v) NegWrongOp))))]

   ; StrLenWrongOp
   [-->meta (θ_1 : ((\# v) StrLenWrongOp))
            (θ_2 : e)
        
            E-StrLenWrongOp
            
            (where (θ_2 : e)
                   (str_len_wrong_op (θ_1 : ((\# v) StrLenWrongOp))))]
   
   
   ; abnormal expressions with relational operators
   ; EqFail
   [-->meta (θ_1 : ((v_1 == v_2) EqFail))
            (θ_2 : e)
        
            E-EqFail
            
            (where (θ_2 : e)
                   (eq_fail (θ_1 : ((v_1 == v_2) EqFail))))]

   ; OrdCompWrongOps
   [-->meta (θ_1 : ((v_1 < v_2) OrdCompWrongOps))
            (θ_2 : e)
        
            E-OrdCompWrongOpsLt
            
            (where (θ_2 : e)
                   (ord_comp_wrong_ops_lt (θ_1 : ((v_1 < v_2) OrdCompWrongOps))))]

   [-->meta (θ_1 : ((v_1 <= v_2) OrdCompWrongOps))
            (θ_2 : e)
        
            E-OrdCompWrongOpsLe
            
            (where (θ_2 : e)
                   (ord_comp_wrong_ops_le (θ_1 : ((v_1 <= v_2) OrdCompWrongOps))))] 


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
   [-->meta (θ_1 : (((v_1 \[ v_2 \]) = v_3) NonTable))
            (θ_2 : s)
        
            E-NonTableStat
            
            (where (θ_2 : s)
                   (non_table_s (θ_1 : (((v_1 \[ v_2 \]) = v_3) NonTable))))]

   [-->meta (θ_1 : (((v_1 \[ v_2 \]) = v_3) WrongKey))
            (θ_2 : s)
        
            E-WrongKeyStat
            
            (where (θ_2 : s)
                   (wrong_key_s (θ_1 : (((v_1 \[ v_2 \]) = v_3) WrongKey))))] 

   ))

(provide meta)
