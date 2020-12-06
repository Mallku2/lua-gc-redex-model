#lang racket
; Expressions that don't interact with some store

(require redex
         "../grammar.rkt"
         "../Meta-functions/grammarMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         racket/format ; String conversion
         )

(define terms-rel
  (reduction-relation
   ext-lang                                             
   #:domain (side-condition any (is_term? (term any)))

   ; tuples
   [--> (in-hole Et (< v_1 v_2 ... >))
        (in-hole Et v_1)
        E-TruncateNonEmptyTuple]

   [--> (in-hole Et (< >))
        (in-hole Et nil)
        E-TruncateEmptyTuple]
   
   [--> (in-hole Eu (< v_1 v_2 ... >))
        (fixUnwrap Eu (v_1 v_2 ...))
        E-UnwrapNonEmptyTuple]

   [--> (in-hole Eu (< >))
        (fixUnwrap Eu '())
        E-UnwrapEmptyTuple]
   ;                                                                                          
   ;                                                             ;                            
   ;                                                                                          
   ;                                                                                          
   ;                                                                                          
   ;    ;;;;   ;;  ;;  ;;;;;    ;;;;    ;;;;    ;;;;    ;;;;   ;;;      ;;;;   ; ;;;    ;;;;  
   ;   ;;  ;;   ;  ;   ;;  ;;   ;;  ;  ;;  ;;  ;    ;  ;    ;    ;     ;;  ;;  ;;   ;  ;    ; 
   ;   ;    ;    ;;    ;    ;   ;      ;    ;  ;       ;         ;     ;    ;  ;    ;  ;      
   ;   ;;;;;;    ;;    ;    ;   ;      ;;;;;;   ;;;;    ;;;;     ;     ;    ;  ;    ;   ;;;;  
   ;   ;         ;;    ;    ;   ;      ;            ;       ;    ;     ;    ;  ;    ;       ; 
   ;   ;;   ;   ;  ;   ;;  ;;   ;      ;;   ;  ;    ;  ;    ;    ;     ;;  ;;  ;    ;  ;    ; 
   ;    ;;;;   ;;  ;;  ;;;;;    ;       ;;;;    ;;;;    ;;;;   ;;;;;    ;;;;   ;    ;   ;;;;  
   ;                   ;                                                                      
   ;                   ;                                                                      
   ;                   ;                                                                      

   ; Operator ()
   ; Defined like this, and not in terms of evaluation contexts, to avoid
   ; shift/reduce and reduce/reduce conflicts
   [--> (\( (< v_1 v ... >) \))
        v_1
        E-ParenthesOnNonEmptyTuple]
   
   [--> (\( (< >) \))
        nil
        E-ParenthesOnEmptyTuple]
   
   [--> (\( v \))
        v
        E-ParenthesisOnValue]
   
   ; Primitive Operations
   ; Binary operations
   [--> (v_1 binop v_2)
        (δ (binop v_1 v_2))
        E-BinOpNumber
        
        (side-condition (and (is_number? (term v_1))
                             (is_number? (term v_2))
                             (or (term (isArithBinOp binop))
                                 (term (isRelationalOperator binop)))))]

   [--> (v_1 binop v_2)
        (δ (binop v_1 v_2))
        E-BinOpString
        
        (side-condition (and (is_string? (term v_1))
                             (is_string? (term v_2))
                             (or (term (isRelationalOperator binop))
                                 (is_strconcat? (term binop)))))]
   
   ; Logical conectives
   [--> (v binop e)
        (δ (binop v e))
        E-LogicOp
        
        (side-condition (term (isBooleanBinOp binop)))]
   
   ; Unary operations
   [--> (unop v)
        (δ (unop v))
        E-UnOp
        
        (side-condition (or (and (is_string? (term v))
                                 (equal? (term unop) (term \#)))
                            
                            (and (is_number? (term v))
                                 (equal? (term unop) (term -)))
                            
                            (equal? (term unop) (term not))))]
   
   ; Equality comparison
   [--> (v_1 == v_2)
        true
        E-EqualitySuccess
        (side-condition (is_true? (term (δ (== v_1 v_2)))))]

   [--> (v_1 == v_2)
        false
        E-EqualityFailNtables
        (side-condition (is_false? (term (δ (== v_1 v_2)))))
        (side-condition (or (not (is_tid? (term v_1)))
                            (not (is_tid? (term v_2)))))]
   
   ; Translation of expressions involving > and >=
   [--> (v_1 binop v_2)
        (v_2 any v_1)
        E-TranslateComparison
        
        (side-condition (or (equal? (term binop) (term >))
                            (equal? (term binop) (term >=))))
        (where any (translateComparisonOp binop))]
   
   ; Coercion
   [--> (v_1 binop v_2)
        (e_1 binop e_2)
        E-ArithBinOpCoercion
        
        (side-condition (and
                         ; the following condition triggers coercion
                         (or (not (is_number? (term v_1)))
                             (not (is_number? (term v_2))))
                         
                         (term (isArithBinOp binop))))
        
        (where e_1 (δ (tonumber v_1 nil)))
        (where e_2 (δ (tonumber v_2 nil)))

        (side-condition (not (or (is_nil? (term e_1))
                                 (is_nil? (term e_2)))))]
   
   [--> (- v)
        (- e)

        (side-condition (not (is_number? (term v))))

        E-NegationCoercion
        
        (where e (δ (tonumber v nil)))

        (side-condition (not (is_nil? (term e))))]

   [--> (v_1 .. v_2)
        (any_1 .. any_2)
        E-StringConcatCoercion
        
        (side-condition (and
                         ; the following condition triggers coercion
                         (or (not (is_string? (term v_1)))
                             (not (is_string? (term v_2))))

                         (or (is_string? (term v_1))
                             (is_number? (term v_1)))
                         
                         (or (is_string? (term v_2))
                             (is_number? (term v_2)))))
        ; No need to look into the store for
        ; information about the given values, for
        ; coercion purposes
        (where any_1 (δ (tostring v_1 ())))
        (where any_2 (δ (tostring v_2 ())))]
   
   ; Abnormal situations with primitive operators
   [--> (v_1 binop v_2)
        ((v_1 binop v_2)ArithWrongOps)
        E-ArithBinOpWrongOps
        
        (side-condition (and (or (not (is_number? (term v_1)))
                                 (not (is_number? (term v_2))))
                         (term (isArithBinOp binop))))
        
        (where any_1 (δ (tonumber v_1 nil)))
        (where any_2 (δ (tonumber v_2 nil)))

        ; e cover the case of hexadecimal numbers, with binary exponents, wich
        ; are translated into the expression nmbr * 2 ^ exp
        (side-condition (or (not (is_e? (term any_1))) 
                            (is_nil? (term any_1))
                            
                            (not (is_e? (term any_2)))
                            (is_nil? (term any_2))))
        ]
   
   [--> (- v)
        ((- v)NegWrongOp)
        E-AlertNegationWrongOperand
        
        (side-condition (not (is_number? (term v))))
        (where any (δ (tonumber v nil)))
        
        (side-condition (or (not (is_e? (term any)))
                            (is_nil? (term any))))]
   
   [--> (v_1 .. v_2)
        ((v_1 .. v_2)StrConcatWrongOps)
        E-AlertStringConcatWrongOperands
        
        (side-condition (or (not (or (is_string? (term v_1))
                                     (is_number? (term v_1))))
                            
                            (not (or (is_string? (term v_2))
                                     (is_number? (term v_2))))))]
   
   [--> (\# v)
        ((\# v)StrLenWrongOp)
        E-AlertStringLengthWrongOperand
        
        (side-condition (not (is_string? (term v))))]
   
   [--> (v_1 == v_2)
        ((v_1 == v_2)EqFail)
        
        E-AlertEqualityFail
        
        (side-condition (is_false? (term (δ (== v_1 v_2)))))
        (side-condition (and (is_tid? (term v_1))
                             (is_tid? (term v_2))))]
   
   [--> (v_1 binop v_2)
        ((v_1 binop v_2)OrdCompWrongOps)
        E-AlertOrdCompWrongOps
        
        (side-condition (and (term (isRelationalOperator binop))
                             
                             (not (and (equal? (term (δ (type v_1)))
                                               (term (δ (type v_2))))
                                       
                                       (or (is_string? (term v_1))
                                           (is_number? (term v_1)))))))
        ]


   ; built-in services
     [--> ($builtIn builtinserv (v ...))
          (δ (builtinserv v ...))

        (side-condition (member (term builtinserv)
                                (term (; basic functions
                                       assert
                                       error
                                       load
                                       pcall
                                       rawequal
                                       select
                                       tonumber
                                       type
                                       xpcall
                                       ; math
                                       math.abs
                                       math.acos
                                       math.asin
                                       math.atan
                                       math.ceil
                                       math.cos
                                       math.cosh
                                       math.deg
                                       math.exp
                                       math.floor
                                       math.fmod
                                       math.log
                                       math.max
                                       math.modf
                                       math.rad
                                       math.sin
                                       math.sinh
                                       math.sqrt
                                       math.tan
                                       math.tanh
                                       ; string
                                       string.len
                                       string.rep
                                       string.reverse
                                       string.sub
                                       ; table
                                       table.pack
                                       ))))
        E-BuiltInTerm]
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


   ; If statement
   [--> (if v then s_1 else s_2 end)
        s_1
        IfTrue
      
        (side-condition (not (is_false_cond? (term v))))]
   
   [--> (if v then s_1 else s_2 end)
        s_2
        IfFalse
      
        (side-condition (is_false_cond? (term v)))]

   ; While statement
   [--> (while e do s end)
        (($iter e do s end) Break)
        SignpostWhile]
   
   [--> ($iter e do ssing end)
        (if e then (ssing ($iter e do ssing end)) else \; end)
        While_single_stat]

   [--> ($iter e do (ssing_1 ssing_2 ssing_3 ...) end)
        (if e then
            (ssing_1 ssing_2 ssing_3 ...
                     ($iter e do (ssing_1 ssing_2 ssing_3 ...) end))
            else \; end)
        While_conc_stats]

   
   ; Concatenation of statements
   ; This added rule has to do with the concrete grammar used
   ; in this mechanization.
   [--> (\; ssing)
        ssing
        ConcatBehavior]

   [--> (\; ssing_1 ssing_2 ssing_3 ...)
        (ssing_1 ssing_2 ssing_3 ...)
        ConcatBehavior2]

   ; Do ... End block
   [--> (do \; end)
        \;
        DoEnd]

   ; List length-equating rules for assignment statements
   ; The rule only make sense when there are 2 or more r-values (spec. useful
   ; for redex-check'ing purposes)
   [--> (evar ... = v_1 v_2 v_3 ...)
        (evar ... = v_4 ...)
        AssignDiscardRvalues
        
        (where Number_1 ,(length (term (evar ...))))
        (where Number_2 ,(length (term (v_1 v_2 v_3 ...))))
        
        (side-condition (< (term Number_1) (term Number_2)))
        
        (where (v_4 ...) ,(take (term (v_1 v_2 v_3 ...)) (term Number_1)))
        ]

   [--> (evar_1 evar_2 evar_3 ... = v_1 ...)
        (evar_1 evar_2 evar_3 ... = v_2 ...)
        AssignCompleteRvalues
        
        (where Number_1 ,(length (term (evar_1 evar_2 evar_3 ...))))
        (where Number_2 ,(length (term (v_1 ...))))
        
        (side-condition (> (term Number_1) 
                           (term Number_2)))

        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (- (term Number_1) (term Number_2))
                                             (term nil))))
        ]

   [--> (evar_1 evar_2 ... evar_3 = v_1 v_2 ... v_3)
        ((evar_3 = v_3) (evar_1 evar_2 ... = v_1 v_2 ...))
        AssignSplit
        
        (side-condition (= (length (term (evar_1 evar_2 ... evar_3)))
                           (length (term (v_1 v_2 ... v_3)))))]

   [--> (local Name_1 Name_2 ... = v_1 v_2 v_3 ... in s end)
        (local Name_1 Name_2 ... = v_4 ... in s end)
        
        LocalDiscardRvalues
        
        (where Number_1 ,(length (term (Name_1 Name_2 ...))))
        (where Number_2 ,(length (term (v_1 v_2 v_3 ...))))
        
        (side-condition (< (term Number_1) (term Number_2)))

        (where (v_4 ...) ,(take (term (v_1 v_2 v_3 ...)) (term Number_1)))]

   [--> (local Name_1 Name_2 Name_3 ... = v_1 ... in s end)
        (local Name_1 Name_2 Name_3 ... = v_2 ... in s end)
        
        LocalCompleteRvalues

        (where Number_1 ,(length (term (Name_1 Name_2 Name_3 ...))))
        (where Number_2 ,(length (term (v_1 ...))))
        
        (side-condition (> (term Number_1) (term Number_2)))
        
        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (- (term Number_1) (term Number_2))
                                             (term nil))))
        ]

   ; Break
   [--> ((in-hole Elf break) Break)
        \;
        Break]

   [--> (\; Break)
        \;
        FinalizationWhile]

   ; Call over a non-function value
   [--> (v (v_1 ...))
        ((v (v_1 ...))WrongFunCall)

        E-AlertWrongFunCall
        ; Determine that v_1 is not a reference to a function
        (side-condition (not (is_cid? (term v))))]

   [--> ($statFunCall v (v_1 ...))
        (($statFunCall v (v_1 ...))WrongFunCall)

        E-AlertWrongStatFunCall
        ; Determine that v_1 is not a reference to a function
        (side-condition (not (is_cid? (term v))))]
   
   ; Method call
   [--> (v : Name (e ...))
        ((v \[ String \]) (v e ...))
        E-MethodCall

        (where String ,(symbol->string (term Name)))]

   [--> ($statFunCall v : Name (e ...))
        ($statFunCall (v \[ String \]) (v e ...))
        E-MethodCallStat

        (where String ,(symbol->string (term Name)))]

   ; Return
   [--> ((in-hole Elf (return v ...)) (renv ...) RetStat)
        \;
        E-DiscardValues]

   [--> (\; (renv ...) RetStat)
        \;
        E-ReturnNoValues]

   [--> ((in-hole Elf (return v ...)) (renv ...) RetExp)
        (< v ... >)
        E-ReturnValues]

   [--> (\; (renv ...) RetExp)
        (< >)
        E-ReturnEmptyTuple]

   
   [--> ((in-hole Elf (return v ...)) Break)
        (return v ...)
        E-InteractionReturnBreak]

   ; loc. variables
    [--> (\; (renv ...) LocalBody)
        \;
        E-RemoveLocal]
   
   ; Protected mode
   [--> ((in-hole Enp ($err v)) ProtectedMode)
        (< false v >)
        
        E-ProtectedModeErrorCatched]

   [--> ((in-hole Enp ($err v_1)) ProtectedMode v_2)
        ((in-hole Enp ($err "error in error handling")) ProtectedMode)
        
        E-XProtectedModeErrorCatchedNoHandler

        (side-condition (not (is_cid? (term v_2))))]

   [--> ((in-hole Enp ($err v)) ProtectedMode cid)
        (cid (v))
        
        E-XProtectedModeErrorCatchedHandler]
   
   [--> ((< v ... >) ProtectedMode)
        (< true (< v ... >) >)
        
        E-ProtectedModeNoErrorWithReturnedValues]

   [--> ((< v_1 ... >) ProtectedMode v_2)
        (< true (< v_1 ... >) >)
        
        E-XProtectedModeNoErrorWithReturnedValues]
   ))

(provide terms-rel)
