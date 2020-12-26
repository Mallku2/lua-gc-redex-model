#lang racket
(require redex
         "../grammar.rkt")

; substitution function over expressions
; PARAMS:
; exp : the expression to which the substitution is applied
; parameters : a list of identifiers to be subtituted
; explist : a list of expressions to substitute the identifiers in
; parameters. The identifier in the position i of parameters will
; be replaced by the expression in the position i of expressions
(define-metafunction ext-lang
  substExp : e ((id e) ... ) -> e

  ; Empty mapping
  [(substExp e ())
   e]
  
  ; Variable identifier or vararg expression
  [(substExp id_1 ((id_2 e) ...))
   (applySubst id_1 ((id_2 e) ...))]
  
  ; Function call
  [(substExp (e_1 (e_2 ...)) ((id e_3) ...))
   ((substExp e_1 ((id e_3) ...))
    ((substExp e_2 ((id e_3) ...)) ...))]
  
  [(substExp (e_1 : Name (e_2 ...)) ((id e_3) ...))
   ((substExp e_1 ((id e_3) ...))
    : Name ((substExp e_2 ((id e_3) ...)) ...))]
  
  ; Built-in procedure call
  [(substExp ($builtIn builtinserv (e_1 ...)) ((id e_2) ...))
   ($builtIn builtinserv ((substExp e_1 ((id e_2) ...)) ...))]
  
  ; Operator '( ')'
  [(substExp (\( e_1 \)) ((id e_2) ...))
   (\( (substExp e_1 ((id e_2) ...)) \))]
  
  ; Table indexing
  [(substExp (e_1 \[ e_2 \]) ((id e_3) ...))
   ((substExp e_1 ((id e_3) ...)) \[ (substExp e_2 ((id e_3) ...)) \])]
  
  ; Tuple
  [(substExp (< >) ((id e) ...))
   (< >)]
  ; Non empty tuple
  [(substExp (< e_1 ... >) ((id e_2) ...))
   (< (substExp e_1 ((id e_2) ...)) ... >)]
  
  ; function definition
  ; we are assuming that the identifiers parlist occur in the 
  ; same order as in namelist
  ; when the substitution defines a substitute to a vararg expression, it is
  ; discarded
  [(substExp (function Name parameters s end) ((id_1 e_1) ...
                                               (<<< e_2)
                                               (id_2 e_3) ...))
   (substExp (function Name parameters s end) ((id_1 e_1) ... (id_2 e_3) ...))]
  
  [(substExp (function Name parameters s end) ((id_1 e_1) ...))
   (function Name parameters 
             (substBlock s ((id_2 e_2) ...))
             end)
   (where ((id_2 e_2) ...) ,(remove* (term parameters) (term ((id_1 e_1) ...))
                                     (λ (identifier pair)
                                       (equal? identifier
                                               (list-ref pair 0)))))]
  
  
  ; table constructor
  [(substExp (\{ field ... \}) ((id e) ...))
   (\{ (substField field ((id e) ...)) ... \})]
  
  ; binary operators
  [(substExp (e_1 binop e_2) ((id e) ...))
   ((substExp e_1 ((id e) ...))
    binop
    (substExp e_2 ((id e) ...)))]
  
  ; unary operators
  [(substExp (unop e_1) ((id e_2) ...))
   (unop (substExp e_1 ((id e_2) ...)))]
  
  ;                                                                  
  ;                                             ;                    
  ;                                                                  
  ;                                     ;                            
  ;                                     ;                            
  ;    ;;;;   ;    ;  ; ;;;           ;;;;;;  ;;;     ;;;;;;;  ;;;;  
  ;    ;;  ;  ;    ;  ;;   ;            ;       ;     ;  ;  ; ;;  ;; 
  ;    ;      ;    ;  ;    ;            ;       ;     ;  ;  ; ;    ; 
  ;    ;      ;    ;  ;    ;   ;;;      ;       ;     ;  ;  ; ;;;;;; 
  ;    ;      ;    ;  ;    ;            ;       ;     ;  ;  ; ;      
  ;    ;      ;   ;;  ;    ;            ;       ;     ;  ;  ; ;;   ; 
  ;    ;       ;;; ;  ;    ;             ;;;  ;;;;;   ;  ;  ;  ;;;;  
  ;                                                                  
  ;                                                                  
  ;
  
  [(substExp (s (renv ...) RetExp) ((id e) ...))
   ((substBlock s ((id e) ...)) (renv ...) RetExp)]
  
  [(substExp ($err v) ((id e) ...))
   ($err v)]

  [(substExp ((v_1 \[ v_2 \])NonTable) ((id e) ...))
   ((v_1 \[ v_2 \])NonTable)]

  [(substExp ((objref \[ v \])WrongKey) ((id e) ...))
   ((objref \[ v \])WrongKey)]
  
  [(substExp ((v_1 arithop v_2) ArithWrongOps) ((id e) ...))
   ((v_1 arithop v_2) ArithWrongOps)]

  [(substExp ((v_1 .. v_2) StrConcatWrongOps) ((id e) ...))
   ((v_1 .. v_2) StrConcatWrongOps)]
     
  [(substExp ((v_1 < v_2) OrdCompWrongOps) ((id e) ...))
   ((v_1 < v_2) OrdCompWrongOps)]

  [(substExp ((v_1 <= v_2) OrdCompWrongOps) ((id e) ...))
   ((v_1 <= v_2) OrdCompWrongOps)]

  [(substExp ((- v)NegWrongOp) ((id e) ...))
   ((- v)NegWrongOp)]
  
  [(substExp ((\# v)StrLenWrongOp) ((id e) ...))
   ((\# v)StrLenWrongOp)]

  [(substExp ((v_1 == v_2)EqFail) ((id e) ...))
   ((v_1 == v_2)EqFail)]

  [(substExp ((v_1 relop v_2)OrdCompWrongOps) ((id e) ...))
   ((v_1 relop v_2)OrdCompWrongOps)]

  [(substExp (e_1 ProtectedMode) ((id e_2) ...))
   ((substExp e_1 ((id e_2) ...)) ProtectedMode)]

  [(substExp (e_1 ProtectedMode v) ((id e_2) ...))
   ((substExp e_1 ((id e_2) ...)) ProtectedMode v)]
  
  ; These case holds for every expression without an structure, different than
  ; a variable or a vararg exp: nil, empty, boolean, number, string, 
  ; simpvalref, objref
  [(substExp any ((id e_2) ...))
   any]
  )

(provide substExp)

; Substitution function for blocks
(define-metafunction ext-lang
  substBlock : s ((id e) ...) -> s

  ; Empty mapping
  [(substBlock s ())
   s]
  
  ; Empty statement
  [(substBlock \; ((id e) ...))
   \;]

  ; Function call
  [(substBlock ($statFunCall e_1 (e_2 ...)) ((id e_3) ...))
   ($statFunCall e_4 (e_5 ...))

   (where (e_4 (e_5 ...)) (substExp (e_1 (e_2 ...)) ((id e_3) ...)))]

  [(substBlock ($statFunCall e_1 : Name (e_2 ...)) ((id e_3) ...))
   ($statFunCall e_4 : Name (e_5 ...))

   (where (e_4 : Name (e_5 ...))
          (substExp (e_1 : Name (e_2 ...)) ((id e_3) ...)))]

  ; Built-in procedure call
  [(substBlock ($builtIn builtinserv (e_1 ...)) ((id e_2) ...))
   (substExp ($builtIn builtinserv (e_1 ...)) ((id e_2) ...))]

  ; Concatenation of statements
  [(substBlock (s_1 s_2 ...) ((id e) ...))
   ((substBlock s_1 ((id e) ...))
    (substBlock s_2 ((id e) ...)) ...)]

  ; Block Do...End
  [(substBlock (do s end) ((id e) ...))
   (do (substBlock s ((id e) ...)) end)]

  ; Break statement
  [(substBlock break ((id e_2) ...))
   break]

  ; Return statement
  [(substBlock (return e_1 ...) ((id e_2) ...))
   (return (substExp e_1 ((id e_2) ...)) ...)]

  ; Conditional
  [(substBlock (if e_1 then s_1 else s_2 end) ((id e_2) ...))
   (if (substExp e_1 ((id e_2) ...)) then
       (substBlock s_1 ((id e_2) ...))
       else (substBlock s_2 ((id e_2) ...)) end)]
  
  ; While loop
  [(substBlock (while e_1 do s end) ((id e_2) ...))
   (while (substExp e_1 ((id e_2) ...)) do 
          (substBlock s ((id e_2) ...)) 
          end)]

  ; we can't rule out the possibility of having this construction into the scope
  ; of a local variable def. 
  [(substBlock ($iter e_1 do s end) ((id e_2) ...))
   ($iter (substExp e_1 ((id e_2) ...)) do 
                 (substBlock s ((id e_2) ...)) 
                 end)]
  
  ; local statement
  [(substBlock (local Name_1 ... = e_1 ... in s end) ((id_1 e_2) ...))
   (local Name_1 ... = (substExp e_1 ((id_1 e_2) ...)) ... in
                       (substBlock s ((id_2 e_3) ...)) end)

   ; remove substitutes for the variables already bound by this local stat
   (where ((id_2 e_3) ...) ,(remove* (term (Name_1 ...)) (term ((id_1 e_2) ...))
                                     (λ (identifier pair)
                                       (equal? identifier
                                               (list-ref pair 0)))))]
   
  ; Variable assignment
  [(substBlock (var ... = e_1 ...) ((id e_2) ...))
   ((substExp var ((id e_2) ...)) ...
    =
    (substExp e_1 ((id e_2) ...)) ...)]

  ;                                                                  
  ;                                             ;                    
  ;                                                                  
  ;                                     ;                            
  ;                                     ;                            
  ;    ;;;;   ;    ;  ; ;;;           ;;;;;;  ;;;     ;;;;;;;  ;;;;  
  ;    ;;  ;  ;    ;  ;;   ;            ;       ;     ;  ;  ; ;;  ;; 
  ;    ;      ;    ;  ;    ;            ;       ;     ;  ;  ; ;    ; 
  ;    ;      ;    ;  ;    ;   ;;;      ;       ;     ;  ;  ; ;;;;;; 
  ;    ;      ;    ;  ;    ;            ;       ;     ;  ;  ; ;      
  ;    ;      ;   ;;  ;    ;            ;       ;     ;  ;  ; ;;   ; 
  ;    ;       ;;; ;  ;    ;             ;;;  ;;;;;   ;  ;  ;  ;;;;  
  ;                                                                  
  ;                                                                  
  ;
  [(substBlock ($err v) ((id e) ...))
   ($err v)]
  
  [(substBlock (s Break) ((id e) ...))
   ((substBlock s ((id e) ...)) Break)]

  [(substBlock (((objref \[ v_1 \]) = v_2) WrongKey) ((id e) ...))
   (((objref \[ v_1 \]) = v_2) WrongKey)]

  [(substBlock (((v_1 \[ v_2 \]) = v_3) NonTable) ((id e) ...))
   (((v_1 \[ v_2 \]) = v_3) NonTable)]

  [(substBlock ((v_1 (v_2 ...)) WrongFunCall) ((id e) ...))
   ((v_1 (v_2 ...)) WrongFunCall)]

  [(substBlock (($statFunCall v_1 (v_2 ...)) WrongFunCall) ((id e) ...))
   (($statFunCall v_1 (v_2 ...)) WrongFunCall)]

  ; every variable bound in s has already been replaced by a ref.
  [(substBlock (s (renv ...) LocalBody) ((id e) ...))
   ((substBlock s ((id e) ...)) (renv ...) LocalBody)]

  [(substBlock (s (renv ...) RetStat) ((id e) ...))
   ((substBlock s ((id e) ...)) (renv ...) RetStat)]
  )

(provide substBlock)

; substitution function for img(θ), useful for preparation of terms in
; redex-check
(define-metafunction ext-lang
  substTheta : θ ((id e) ...) -> θ

  [(substTheta () _)
   ()]

  [(substTheta ((cid functiondef_1) osp_1 ...) ((id e) ...))
   ((cid functiondef_2) osp_2 ...)

   (where (osp_2 ...) (substTheta (osp_1 ...) ((id e) ...)))
   (where functiondef_2 (substExp functiondef_1 ((id e) ...)))]

  ; we substitute into the evaluated table and metatable's ref
  [(substTheta ((tid (evaluatedtable_1 any_1 pos)) osp_1 ...) ((id e) ...))
   ((tid (evaluatedtable_1 any_2 pos)) osp_2 ...)

   (where (osp_2 ...) (substTheta (osp_1 ...) ((id e) ...)))
   (where evaluatedtable_2 (substExp evaluatedtable_1 ((id e) ...)))
   (where any_2 (substExp any_1 ((id e) ...)))]
  )

(provide substTheta)

; to ease the use of substBlock and substExp
(define-metafunction ext-lang
  subst : t ((id e) ...) -> t

  [(subst e_1 ((id e_2) ...))
   (substExp e_1 ((id e_2) ...))]

  [(subst s ((id e) ...))
   (substBlock s ((id e) ...))]
  )

(provide subst)
;                                                                  
;                                                                  
;                                                                  
;                              ;     ;;;       ;                   
;     ;;                               ;                           
;     ;;                               ;                           
;     ;;    ;    ;  ;;   ;   ;;;       ;     ;;;     ;;;;    ; ;;; 
;    ;  ;   ;    ;   ;  ;      ;       ;       ;         ;   ;;    
;    ;  ;   ;    ;    ;;       ;       ;       ;         ;   ;     
;   ;;;;;;  ;    ;    ;;       ;       ;       ;     ;;;;;   ;     
;   ;    ;  ;    ;    ;;       ;       ;       ;    ;    ;   ;     
;   ;    ;  ;   ;;   ;  ;      ;       ;       ;    ;   ;;   ;     
;  ;      ;  ;;; ;  ;    ;   ;;;;;   ;;;;;   ;;;;;   ;;; ;   ;     
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(define-metafunction ext-lang
  applySubst : id ((id e) ...) -> e
  
  [(applySubst id ())
   id]
  
  [(applySubst id_1 ((id_1 e_1) (id e) ...))
   e_1]

  ; {id_1 ≠ id_2}
  [(applySubst id_1 ((id_2 e_1) (id e) ...))
   (applySubst id_1 ((id e) ...))]
  )

; auxiliar meta-function to perform a substitution over a table field
(define-metafunction ext-lang
  substField : field ((id e) ...) -> field
  
  [(substField (\[ e_1 \] = e_2) ((id e_3) ...))
   (\[ (substExp e_1 ((id e_3) ...)) \]
        =
        (substExp e_2 ((id e_3) ...)))]
  
  [(substField e_1 ((id e_2) ...))
   (substExp e_1 ((id e_2) ...))])

;                  
;                  
;      ;;          
;     ;            
;     ;            
;     ;            
;   ;;;;;   ;    ; 
;     ;     ;;  ;; 
;     ;      ;  ;  
;     ;      ;  ;  
;     ;      ;;;;  
;     ;       ;;   
;     ;       ;;   
;                  
;                  
;                  
;
(define-metafunction ext-lang
  ; in some contexts, like in wfc's definition, fv will be applied over terms
  ; which are not in s ∪ e; we need to weaken its domain
  fv : any -> (id ...)

  
  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;             ;               ;            
  ;             ;               ;            
  ;    ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
  ;   ;    ;    ;      ;   ;    ;     ;    ; 
  ;   ;         ;          ;    ;     ;      
  ;    ;;;;     ;      ;;;;;    ;      ;;;;  
  ;        ;    ;     ;    ;    ;          ; 
  ;   ;    ;    ;     ;   ;;    ;     ;    ; 
  ;    ;;;;      ;;;   ;;; ;     ;;;   ;;;;  
  ;                                          
  ;                                          
  ;                                          
  ;                                          

  [(fv \;)
   ()]

  [(fv break)
   ()]

  [(fv (return))
   ()]

  [(fv (return any_1 any_2 ...))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv any_1))
   (where ((id_2 ...) ...) ((fv any_2) ...))]

  [(fv (any_1 : Name (any_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv any_1))
   (where ((id_2 ...) ...) ((fv any_2) ...))]

  [(fv ($statFunCall any_1 (any_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv any_1))
   (where ((id_2 ...) ...) ((fv any_2) ...))]

  [(fv ($statFunCall any_1 : Name (any_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv any_1))
   (where ((id_2 ...) ...) ((fv any_2) ...))]

  [(fv ($builtIn builtinserv ()))
   ()]

  [(fv ($builtIn builtinserv (any_1 any_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv any_1))
   (where ((id_2 ...) ...) ((fv any_2) ...))]

  [(fv (var_1 var_2 ... = any ...))
   (id_1 ... id_2 ... ... id_3 ... ...)

   (where (id_1 ...) (fv var_1))
   (where ((id_2 ...) ...) ((fv var_2) ...))
   (where ((id_3 ...) ...) ((fv any) ...))]

  [(fv (do any end))
   (fv any)]

  [(fv (if any_1 then any_2 else any_3 end))
   (id_1 ... id_2 ... id_3 ...)

   (where (id_1 ...) (fv any_1))
   (where (id_2 ...) (fv any_2))
   (where (id_3 ...) (fv any_3))]

  [(fv (while any_1 do any_2 end))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv any_1))
   (where (id_2 ...) (fv any_2))]

  [(fv (local Name ... = any_1 ... in any_2 end))
   (id_4 ...)

   (where ((id_1 ...) ...) ((fv any_1) ...))
   (where (id_2 ...) (fv any_2))
   (where (id_3 ...) ,(remove* (term (Name ...))
                               (term (id_2 ...))))
   (where (id_4 ...) ,(remove-duplicates (term (id_3 ... id_1 ... ...))))]

  [(fv (s_1 s_2 s_3 ...))
   (id_2 ... id_3 ... id_4 ... ...)

   (where (id_2 ...) (fv s_1))
   (where (id_3 ...) (fv s_2))
   (where ((id_4 ...) ...) ((fv s_3) ...))]

  [(fv ($err any))
   (fv any)]

  [(fv ($iter any_1 do any_2 end))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv any_1))
   (where (id_2 ...) (fv any_2))]
  
  ;                                                                          
  ;                                                                          
  ;   ;;;             ;                                                      
  ;     ;             ;                                                      
  ;     ;             ;                         ;               ;            
  ;     ;             ;                         ;               ;            
  ;     ;       ;;;   ;;;;;            ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
  ;     ;      ;   ;  ;;  ;;          ;    ;    ;      ;   ;    ;     ;    ; 
  ;     ;          ;  ;    ;          ;         ;          ;    ;     ;      
  ;     ;      ;;;;;  ;    ;           ;;;;     ;      ;;;;;    ;      ;;;;  
  ;     ;     ;    ;  ;    ;               ;    ;     ;    ;    ;          ; 
  ;     ;     ;   ;;  ;;  ;;          ;    ;    ;     ;   ;;    ;     ;    ; 
  ;      ;;;   ;;; ;  ;;;;;            ;;;;      ;;;   ;;; ;     ;;;   ;;;;  
  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;                                                                          

  [(fv (any (renv ...) LocalBody))
   (fv any)]

  [(fv (any (renv ...) RetStat))
   (fv any)]

  [(fv (any Break))
   (fv any)]

  [(fv (any statlabel))
   (fv any)]
  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;;  ;    ; 
  ;   ;    ;    ;;    ;    ;  ;      
  ;   ;;;;;;    ;;    ;    ;   ;;;;  
  ;   ;         ;;    ;    ;       ; 
  ;   ;;   ;   ;  ;   ;;  ;;  ;    ; 
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;                   ;              
  ;                   ;              
  ;                   ;              
  ;                                  

  [(fv nil)
   ()]

  [(fv Boolean)
   ()]

  [(fv Number)
   ()]

  [(fv String)
   ()]

  [(fv tid)
   ()]

  [(fv cid)
   ()]

  [(fv (function Name_1 (Name_2 ...) any end))
   (id ...)

   (where (id ...) ,(remove* (term (Name_2 ...))
                             (term (fv any))))]

  [(fv (function Name_1 (Name_2 ... <<<) any_1 end))
   (id ...)

   ; there is a non-captured vararg
   (side-condition
    (redex-match ext-lang
                 (side-condition
                  (in-hole C_1 (function Name_3 (Name_4 ...) any_2 end))
                  (member (term <<<)
                          (term (fv any_2))))
                 (term any_1)))
   ; do not remove occurrences of <<<
   (where (id ...) ,(remove* (term (Name_2 ...))
                             (term (fv any_1))))]

  [(fv (function Name_1 (Name_2 ... <<<) any_1 end))
   (id ...)

   ; there is a not a non-captured vararg
   (side-condition
    (not (redex-match ext-lang
                      (side-condition
                       (in-hole C_1 (function Name_3 (Name_4 ...) any_2 end))
                       (member (term <<<)
                               (term (fv any_2))))
                      (term any_1))))
   
   ; remove occurrences of <<<
   (where (id ...) ,(remove* (term (Name_2 ... <<<))
                             (term (fv any_1))))]

  [(fv <<<)
   (<<<)]

  [(fv Name)
   (Name)]

  [(fv (any_1 \[ any_2 \]))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv any_1))
   (where (id_2 ...) (fv any_2))]

  [(fv (\( any \)))
   (fv any)]

  [(fv (\{ field ... \}))
   (id ... ...)

   (where ((id ...) ...) ((fv field) ...))]

  ; Table field with key defined
  [(fv (\[ any_1 \] = any_2))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv any_1))
   (where (id_2 ...) (fv any_2))]

  [(fv (any_1 binop any_2))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv any_1))
   (where (id_2 ...) (fv any_2))]

  [(fv (unop any))
   (fv any)]

  [(fv r)
   ()]

  [(fv (< any ... >))
   (id ... ...)

   (where ((id ...) ...) ((fv any) ...))]

   [(fv (any_1 (any_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv any_1))
   (where ((id_2 ...) ...) ((fv any_2) ...))]

  
  ;                                                                  
  ;                                                                  
  ;   ;;;             ;                                              
  ;     ;             ;                                              
  ;     ;             ;                                              
  ;     ;             ;                                              
  ;     ;       ;;;   ;;;;;            ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;     ;      ;   ;  ;;  ;;          ;;  ;;   ;  ;   ;;  ;;  ;    ; 
  ;     ;          ;  ;    ;          ;    ;    ;;    ;    ;  ;      
  ;     ;      ;;;;;  ;    ;          ;;;;;;    ;;    ;    ;   ;;;;  
  ;     ;     ;    ;  ;    ;          ;         ;;    ;    ;       ; 
  ;     ;     ;   ;;  ;;  ;;          ;;   ;   ;  ;   ;;  ;;  ;    ; 
  ;      ;;;   ;;; ;  ;;;;;            ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;                                                   ;              
  ;                                                   ;              
  ;                                                   ;              
  ;                                                                  

  [(fv (e ProtectedMode))
   (fv e)]

  [(fv (e ProtectedMode v))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv e))
   (where (id_2 ...) (fv v))]

  [(fv (any explabel))
   (fv any)]

  [(fv (any (renv ...) RetExp))
   (fv any)]
  )

(provide fv)