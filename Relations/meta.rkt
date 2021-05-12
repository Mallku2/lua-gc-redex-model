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
   [-->meta (θ : (($statFCall ..._1 v_1 (v_2 ...)) WFunCall objid ...))
            (θ : t)
        
            E-WFunCall
            
            (where t
                   (w_fun_call (θ : (($statFCall ..._1 v_1 (v_2 ...))
                                     WFunCall
                                     objid ...))))]

   ; NonTable or WrongKey, exp
   [-->meta (θ : ((v_1 \[ v_2 \]) explabel objid ...))
            (θ : e)

            (side-condition (or (redex-match? ext-lang
                                              NonTable
                                              (term explabel))
                                (redex-match? ext-lang
                                              WrongKey
                                              (term explabel))))
        
            (where e
                   (index (θ : ((v_1 \[ v_2 \])
                                explabel
                                objid ...))))
            

            E-Index] 

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
 
   ; NonTable or WrongKey, stat
   [-->meta (θ_1 : (((v_1 \[ v_2 \]) = v_3) statlabel objid ...))
            (θ_2 : s)

            (side-condition (or (redex-match? ext-lang
                                              NonTable
                                              (term statlabel))
                                (redex-match? ext-lang
                                              WrongKey
                                              (term statlabel))))
        
            (where (θ_2 : s)
                   (new_index (θ_1 : (((v_1 \[ v_2 \]) = v_3)
                                      statlabel
                                      objid ...))))
            

            E-NewIndexStat] 

   ))

(provide meta)
