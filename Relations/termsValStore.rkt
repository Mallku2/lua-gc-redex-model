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
   #:domain (side-condition (σ : any) (is_term? (term any)))
   
   ; Implicit dereferencing
   ;
   ; Note that we don't need to mention explicitly the context where this
   ; operation occurs, as the evaluation contexts never allow reduction on
   ; l-values, if they are not fields of tables.
   ; As an example of a situation where evaluation contexts are needed
   ; in order to describe correctly the meaning of a contraction, see
   ; E-ConvertVoidToNilWhereTruncate.
   [--> ((vsp_1 ... (r v) vsp_2 ...) : r)
        ((vsp_1 ... (r v) vsp_2 ...) : v)
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

   
   ; State change
   [--> ((vsp_1 ... (r v_1) vsp_2 ...) : (r = v_2))
        ((vsp_1 ... (r v_2) vsp_2 ...) : \;)
        E-RefMapChange]
   
   ; Local variables
   ; Equal quantity of l-values and r-values (forced by using ..._1 on both
   ; sides)
   [--> (σ_1 : (local Name ..._1 = v ..._1 in s_1 end))
;        (σ_2 : (local Name ... = renv ... in
;                 (substBlock s_1 ((id_1 e_1) ...))
;                 end))
        (σ_2 : ((substBlock s_1 ((id_1 e_1) ...)) (renv ...) LocalBody))
        E-Local

        (where (σ_2 (r ...)) (addSimpVal σ_1 (v ...)))
        
        (where ((id_1 e_1) ...) ,(map (lambda (id ref) (list id ref))
                                      (term (Name ...))
                                      (term (r ...))))

        (where (renv ...) ,(map (lambda (r) (term (rEnv (unquote r))))
                                (term (r ...))))
        ]
   
   ))
(provide terms-val-store)
