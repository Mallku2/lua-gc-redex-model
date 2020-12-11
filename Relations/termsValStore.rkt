#lang racket
; Expressions that interact with the values' store

(require redex
         "../grammar.rkt"
         "../Meta-functions/substitution.rkt"
         "../Meta-functions/valStoreMetafunctions.rkt"
         )


(define terms-val-store
  (reduction-relation
   ext-lang
   ;#:domain (σ : t)
   
   ; implicit dereferencing
   ;
   ; note that we don't need to mention explicitly the context where this
   ; operation occurs, as the evaluation contexts never allow reduction on
   ; l-values, if they are not fields of tables.
   [--> (σ : r)
        ((vsp_1 ... (r v) vsp_2 ...) : v)

        ; defined in this way, to help redex-check to generate correct stores
        (where (vsp_1 ... (r v) vsp_2 ...) σ)
        
        E-RefDeref]
   
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

   
   ; state change
   [--> (σ : (r = v_2))
        ((vsp_1 ... (r v_2) vsp_2 ...) : \;)

        ; defined in this way, to help redex-check to generate correct stores
        (where (vsp_1 ... (r v_1) vsp_2 ...) σ)
        
        E-RefMapChange]
   
   ; local variables
   ; equal quantity of l-values and r-values (forced by using ..._1 on both
   ; sides)
   [--> (σ_1 : (local Name_1 Name_2 ..._1 = v_1 v_2 ..._1 in s_1 end))
        (σ_2 : ((substBlock s_1 ((Name_1 r_1) (Name_2 r_2) ...)) (renv ...)
                                                                 LocalBody))
        E-Local

        (where (σ_2 (r_1 r_2 ...)) (addSimpVal σ_1 (v_1 v_2 ...)))
       
        (where (renv ...) ,(map (lambda (r) (term (rEnv (unquote r))))
                                (term (r_1 r_2 ...))))
        ]
   
   ))
(provide terms-val-store)
