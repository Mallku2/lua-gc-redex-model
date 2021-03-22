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

   ; BinopWO
   [-->meta (θ : ((v_1 binop v_2) BinopWO objid ...))
            (θ : e)
        
            E-ArithWO
            
            (where e
                   (binop_wo (θ : ((v_1 binop v_2)
                                            BinopWO
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
