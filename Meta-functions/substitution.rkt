#lang racket
(require redex
         "../grammar.rkt")

; Substitution function over expressions
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
    (substexplist (e_2 ...) ((id e_3) ...)))]
  
  [(substExp (e_1 : Name (e_2 ...)) ((id e_3) ...))
   ((substExp e_1 ((id e_3) ...))
    : Name (substexplist (e_2 ...) ((id e_3) ...)))]
  
  ; Built-in procedure call
  [(substExp ($builtIn builtinserv (e_1 ...)) ((id e_2) ...))
   ($builtIn builtinserv (substexplist (e_1 ...) ((id e_2) ...)))]
  
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
   ,(append (term (< ))
            (term (substexplist (e_1 ...) ((id e_2) ...)))
            (term ( >)))]
  
  ; Function definition
  ; We are assuming that the identifiers parlist occur in the 
  ; same order as in namelist.
  ; When the substitution defines a substitute to a vararg expression, it is
  ; discarded
  [(substExp (function Name_1 (id ...) s end) ((Name e_1) ... (<<< e_2)))
   (substExp (function Name_1 (id ...) s end) ((Name e_1) ...))]
  
  [(substExp (function Name_1 (id ...) s end) ((Name_2 e_1) ...))
   (function Name_1 (id ...) 
             (substBlock s ((Name_3 e_3) ...))
             end)
   (where ((Name_3 e_3) ...) ,(remove* (term (id ...)) (term ((Name_2 e_1) ...))
                                       (λ (identifier pair)
                                         (equal? identifier
                                                 (list-ref pair 0)))))]
  
  
  ; Table constructor
  ; substfield receives and returns a list of the form (field ...)
  ; so in this case, to reconstruct the original expression that has the form
  ; of a list of symbols, we must escape to scheme code an use the append
  ; function put the result of substfield with the others symbols in one list
  [(substExp (\{ field ... \}) ((id e) ...))
   ,(append (term (\{))
            (append (term (substfield (field ...) ((id e) ...)))
                    (term (\}))))]
  
  ; Binary operators
  [(substExp (e_1 binop e_2) ((id e) ...))
   ((substExp e_1 ((id e) ...))
    binop
    (substExp e_2 ((id e) ...)))]
  
  ; Unary operators
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
   (substslist (s_1 s_2 ...) ((id e) ...))]

  ; Block Do...End
  [(substBlock (do s end) ((id e) ...))
   (do (substBlock s ((id e) ...)) end)]

  ; Break statement
  [(substBlock break ((id e_2) ...))
   break]

  ; Return statement
  [(substBlock (return e_1 ...) ((id e_2) ...))
   ,(append (term (return ))
            (term (substexplist (e_1 ...) ((id e_2) ...))))]

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

  ; We can't rule out the possibility of having this construction into the scope
  ; of a local variable def. 
  [(substBlock ($iter e_1 do s end) ((id e_2) ...))
   ($iter (substExp e_1 ((id e_2) ...)) do 
                 (substBlock s ((id e_2) ...)) 
                 end)]
  
  ; Local statement
  [(substBlock (local Name_1 ... = e_1 ... in s end) ((id_1 e_2) ...))
   ,(append (term (local Name_1 ... = ))
            (term (substexplist (e_1 ...) ((id_1 e_2) ...)))
            (term (in (substBlock s ((id_2 e_3) ...)) end)))
   
   (where ((id_2 e_3) ...) ,(remove* (term (Name_1 ...)) (term ((id_1 e_2) ...))
                                     (λ (identifier pair)
                                       (equal? identifier
                                               (list-ref pair 0)))))]
   
  ; Variable assignment
  [(substBlock (var ... = e_1 ...) ((id e_2) ...))
   ,(append (term (substexplist (var ...) ((id e_2) ...)))
            (term ( = ))
            (term (substexplist (e_1 ...) ((id e_2) ...))))]


  
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

  ; every variable bound in s has already been replaced by a ref.
  [(substBlock (s (renv ...) LocalBody) ((id e) ...))
   ((substBlock s ((id e) ...)) (renv ...) LocalBody)]
  )


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
  
  [(applySubst id_1 ((id_2 e_1) (id e) ...))
   (applySubst id_1 ((id e) ...))

   (side-condition (not (equal? (term id_1)
                                (term id_2))))]
  )

; Auxiliar meta-function to perform a substitution over list
; of exp constructions.
(define-metafunction ext-lang
  substexplist : (e ...) ((id e) ...) -> (e ...)
  
  [(substexplist () ((id e) ...))
   ()]
  
  [(substexplist (e_1) ((id e) ...))
   ((substExp e_1 ((id e) ...)))]
  
  [(substexplist (e_1 e ...) ((id e_2) ...))
   ,(append (term ((substExp e_1 ((id e_2) ...)))) 
            (term (substexplist (e ...) ((id e_2) ...))))])

; Auxiliar meta-function to perform a substitution over list
; of exp constructions.
(define-metafunction ext-lang
  substslist : (s ...) ((id e) ...) -> (s ...)
  
  [(substslist () ((id e) ...))
   ()]
  
  [(substslist (s_1) ((id e) ...))
   ((substBlock s_1 ((id e) ...)))]
  
  [(substslist (s_1 s ...) ((id e_2) ...))
   ,(append (term ((substBlock s_1 ((id e_2) ...)))) 
            (term (substslist (s ...) ((id e_2) ...))))])

; Auxiliar meta-function to perform a substitution over list
; of table fields.
(define-metafunction ext-lang
  substfield : (field ...) ((id e) ...) -> (field ...)
  
  [(substfield () ((id e_2) ...))
   ()]
  
  [(substfield ((\[ e_1 \] = e_2)) ((id e_3) ...))
   ((\[ (substExp e_1 ((id e_3) ...)) \]
        =
        (substExp e_2 ((id e_3) ...))))]
  
  [(substfield (e_1) ((id e_2) ...))
   ((substExp e_1 ((id e_2) ...)))]
  
  [(substfield (field_1 field ...) ((id e) ...))
   ,(append (term (substfield (field_1) ((id e) ...))) 
            (term (substfield (field ...) ((id e) ...))))])

; Export subst meta-function
(provide substBlock
         substExp)


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
  fv : (side-condition any (or (is_e? (term any))
                               (is_s? (term any)))) -> (id ...)

  
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

  [(fv (return e_1 e_2 ...))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv e_1))
   (where ((id_2 ...) ...) ((fv e_2) ...))]

  [(fv (e_1 (e_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv e_1))
   (where ((id_2 ...) ...) ((fv e_2) ...))]

  [(fv (e_1 : Name (e_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv e_1))
   (where ((id_2 ...) ...) ((fv e_2) ...))]

  [(fv ($statFunCall e_1 (e_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv e_1))
   (where ((id_2 ...) ...) ((fv e_2) ...))]

  [(fv ($statFunCall e_1 : Name (e_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv e_1))
   (where ((id_2 ...) ...) ((fv e_2) ...))]

  [(fv ($builtIn builtinserv ()))
   ()]

  [(fv ($builtIn builtinserv (e_1 e_2 ...)))
   (id_1 ... id_2 ... ...)

   (where (id_1 ...) (fv e_1))
   (where ((id_2 ...) ...) ((fv e_2) ...))]

  [(fv (var_1 var_2 ... = e ...))
   (id_1 ... id_2 ... ... id_3 ... ...)

   (where (id_1 ...) (fv var_1))
   (where ((id_2 ...) ...) ((fv var_2) ...))
   (where ((id_3 ...) ...) ((fv e) ...))]

  [(fv (do s end))
   (fv s)]

  [(fv (if e then s_1 else s_2 end))
   (id_1 ... id_2 ... id_3 ...)

   (where (id_1 ...) (fv e))
   (where (id_2 ...) (fv s_1))
   (where (id_3 ...) (fv s_2))]

  [(fv (while e do s end))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv e))
   (where (id_2 ...) (fv s))]

  [(fv (local Name ... = e ... in s end))
   (id_4 ...)

   (where ((id_1 ...) ...) ((fv e) ...))
   (where (id_2 ...) (fv s))
   (where (id_3 ...) ,(remove* (term (Name ...))
                               (term (id_2 ...))))
   (where (id_4 ...) ,(remove-duplicates (term (id_3 ... id_1 ... ...))))]

  [(fv (s_1 s_2 ...))
   (id_2 ... id_3 ... ...)

   (where (id_2 ...) (fv s_1))
   (where ((id_3 ...) ...) ((fv s_2) ...))]

  [(fv ($err e))
   (fv e)]

  [(fv ($iter e do s end))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv e))
   (where (id_2 ...) (fv s))]
  
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

  [(fv (s (renv ...) LocalBody))
   (fv s)]

  [(fv (s (renv ...) RetStat))
   (fv s)]

  [(fv (s Break))
   (fv s)]

  [(fv (s statlabel))
   (fv s)]
  
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

  [(fv objid)
   ()]

  [(fv (function Name_1 (Name_2 ...) s end))
   (id ...)

   (where (id ...) ,(remove* (term (Name_2 ...))
                             (term (fv s))))]

  [(fv (function Name_1 (Name_2 ... <<<) s end))
   (id ...)

   (where (id ...) ,(remove* (term (Name_2 ... <<<))
                             (term (fv s))))]

  [(fv <<<)
   (<<<)]

  [(fv Name)
   (Name)]

  [(fv (e_1 \[ e_2 \]))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv e_1))
   (where (id_2 ...) (fv e_2))]

  [(fv (\( e \)))
   (fv e)]

  [(fv (\{ field ... \}))
   (id ... ...)

   (where ((id ...) ...) ((fv field) ...))]

  ; Table field with key defined
  [(fv (\[ e_1 \] = e_2))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv e_1))
   (where (id_2 ...) (fv e_2))]

  [(fv (e_1 binop e_2))
   (id_1 ... id_2 ...)

   (where (id_1 ...) (fv e_1))
   (where (id_2 ...) (fv e_2))]

  [(fv (unop e))
   (fv e)]

  [(fv r)
   ()]

  [(fv (< e ... >))
   (id ... ...)

   (where ((id ...) ...) ((fv e) ...))]

  
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

  [(fv (e explabel))
   (fv e)]

  [(fv (s (renv ...) RetExp))
   (fv s)]
  )

(provide fv)