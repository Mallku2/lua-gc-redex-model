#lang racket
; Expressions that interact with the values' store

(require redex
         "../grammar.rkt"
         "../Meta-functions/substitution.rkt"
         "../Meta-functions/valStoreMetaFunctions.rkt"
         "../Meta-functions/delta.rkt"
         )


(define terms-val-obj-store
  (reduction-relation
   ext-lang
   #:domain (σ : θ : t)
   #:arrow -->σ/θ
   
   ;                                                          
   ;      ;;                                   ;;;     ;;;    
   ;     ;                                       ;       ;    
   ;     ;                                       ;       ;    
   ;     ;                                       ;       ;    
   ;   ;;;;;   ;    ;  ; ;;;     ;;;     ;;;     ;       ;    
   ;     ;     ;    ;  ;;   ;   ;   ;   ;   ;    ;       ;    
   ;     ;     ;    ;  ;    ;  ;            ;    ;       ;    
   ;     ;     ;    ;  ;    ;  ;        ;;;;;    ;       ;    
   ;     ;     ;    ;  ;    ;  ;       ;    ;    ;       ;    
   ;     ;     ;   ;;  ;    ;   ;   ;  ;   ;;    ;       ;    
   ;     ;      ;;; ;  ;    ;    ;;;    ;;; ;     ;;;     ;;; 
   ;                                                          
   ;                                                          
   ;                                                          
   ; function call
   [-->σ/θ (σ_1 : (osp_1 ...
                   (cid (function Name_1 (Name_2 ...) s_1 end))
                   osp_2 ...)
                : ($statFunCall ..._1 cid (v ...)))
         
           (σ_2 : (osp_1 ...
                   (cid (function Name_1 (Name_2 ...) s_1 end))
                   osp_2 ...)
                : ((substBlock s_1 ((Name_2 r) ...))
                   ((rEnv r) ...) any))

           E-ApplyNonVararg

           ; the following 3 patterns are mutually exclusive 
           (where (; formal parameters > actual parameters
                   (Name_3 ..._2 Name_4 Name_5 ... v_1 ..._2) ...
                   ; formal parameters < actual parameters
                   (Name_6 ..._3 v_2 ..._3 v_3 v_4 ...) ...
                   ; formal parameters = actual parameters
                   (Name_7 ..._4 v_5 ..._4) ...)
               
                  ((Name_2 ... v ...)))
        
           ; add nil values, if needed
           ; if formal parameters <= actual parameters =>
           ; ((Name_4 Name_5 ...) ...) = ()
           (where ((Name_8 ...) ...) ((Name_4 Name_5 ...) ...))
           ; this step is needed to flatten the s-expression
           (where (Name_9 ..._10) (Name_8 ... ...))
           (where (v_6 ...) (nil ..._10))

           ; add, to the value store, mappings to the actual parameters
           ; only one (among (v_i ...) ...) is non-empty
           (where ((v_7 ...) ...) ((v_1 ...) ... (v_2 ...) ... (v_5 ...) ...))
           ; this step is needed to flatten the s-expression
           (where (v_8 ...) (v_7 ... ...))
           (where (σ_2 (r ...)) (addSimpVal σ_1 (v_8 ...
                                                 v_6 ...)))

           ; choose apropriate function call label
           ; TODO: just to avoid the duplication of semantic rules
           (where any ,(if (= (length (term ($statFunCall ..._1))) 1)
                           (term RetStat)
                           (term RetExp)))]
   
   ; vararg function call
   [-->σ/θ (σ_1 : (osp_1 ...
                   (cid (function Name_1 (Name_2 ... <<<) s_1 end))
                   osp_2 ...)
                : ($statFunCall ..._1 cid (v ...)))
         
           (σ_2 : (osp_1 ...
                   (cid (function Name_1 (Name_2 ... <<<) s_1 end))
                   osp_2 ...)
                : ((substBlock s_1 ((Name_2 r) ...
                                    (<<< (< v_10 ... >))))
                   ((rEnv r) ...) any))

           E-ApplyVararg

           ; the following 3 patterns are mutually exclusive 
           (where (; formal parameters > actual parameters
                   (Name_3 ..._2 Name_4 Name_5 ... v_1 ..._2) ...
                   ; formal parameters < actual parameters
                   (Name_6 ..._3 v_2 ..._3 v_3 v_4 ...) ...
                   ; formal parameters = actual parameters
                   (Name_7 ..._4 v_5 ..._4) ...)
               
                  ((Name_2 ... v ...)))
        
           ; add nil values, if needed
           ; if formal parameters <= actual parameters =>
           ; ((Name_4 Name_5 ...) ...) = ()
           (where ((Name_8 ...) ...) ((Name_4 Name_5 ...) ...))
           ; this step is needed to flatten the s-expression
           (where (Name_9 ..._10) (Name_8 ... ...))
           (where (v_6 ...) (nil ..._10))

           ; add, to the value store, mappings to the actual parameters
           ; only one (among (v_i ...) ...) is non-empty
           (where ((v_7 ...) ...) ((v_1 ...) ... (v_2 ...) ... (v_5 ...) ...))
           ; this step is needed to flatten the s-expression
           (where (v_8 ...) (v_7 ... ...))
           (where (σ_2 (r ...)) (addSimpVal σ_1 (v_8 ...
                                                 v_6 ...)))

           ; prepare surplus actual parameters, to map them to <<<
           (where ((v_9 ...) ...) ((v_3 v_4 ...) ...))
           ; this step is needed to flatten the s-expression
           (where (v_10 ...) (v_9 ... ...))

           ; choose apropriate function call label
           ; TODO: just to avoid the duplication of semantic rules
           (where any ,(if (= (length (term ($statFunCall ..._1))) 1)
                           (term RetStat)
                           (term RetExp)))] 
   
   ; built-in services
   [-->σ/θ (σ_1 : θ : ($builtIn print (v ...)))
           (σ_2 : θ : nil)
        
           E-BuiltInSigmaWrite
        
           (where σ_2 (δ print v ... σ_1 θ))]
   ))
(provide terms-val-obj-store)
