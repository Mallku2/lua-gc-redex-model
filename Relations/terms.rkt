#lang racket
; Expressions that don't interact with some store

(require redex
         "../grammar.rkt"
         "../Meta-functions/grammarMetaFunctions.rkt"
         "../Meta-functions/delta.rkt"
         )

(define terms-rel
  (reduction-relation
   ext-lang                                             
   ;#:domain t
   #:arrow -->s/e

   ; tuples
   [-->s/e (in-hole Et (< v_1 v_2 ... >))
           (in-hole Et v_1)
           E-TruncateNonEmptyTuple]

   [-->s/e (in-hole Et (< >))
           (in-hole Et nil)
           E-TruncateEmptyTuple]
   
   [-->s/e (in-hole Eu (< v_1 v_2 ... >))
           (fixUnwrap Eu (v_1 v_2 ...))
           E-UnwrapNonEmptyTuple]

   [-->s/e (in-hole Eu (< >))
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
   [-->s/e (\( (< v_1 v ... >) \))
           v_1
           E-ParenthesOnNonEmptyTuple]
   
   [-->s/e (\( (< >) \))
           nil
           E-ParenthesOnEmptyTuple]
   
   [-->s/e (\( v \))
           v
           E-ParenthesisOnValue]
   
   ; primitive Operations
   ; binary operations
   [-->s/e (v_1 binop v_2)
           v_3
        
           E-BinOpNumber

           ; arith. or .. op, over numeric or string operands
           (side-condition (or (term (isArithBinOp binop))
                               (is_strconcat? (term binop))))

           ; apply binop, check if the operation was successful
           (where v_3 (δ binop v_1 v_2))

           ; we are assuming the soundness of δ: successful arithops only return
           ; numbers, successful concat only returns strings
           (side-condition (or (is_string? (term v_3))
                               (is_number? (term v_3))))]
   
   ; logical conectives
   [-->s/e (v binop e)
           (δ binop v e)
           E-LogicOp
        
           (side-condition (term (isBooleanBinOp binop)))]
   
   ; unary operations
   [-->s/e (- v)
           Number
        
           E-NumNeg

           ; check if the operation was successful
           (where Number (δ - v))]
   
   [-->s/e (unop v)
           (δ unop v)
           E-UnOp

        
           (side-condition (or (and ; there is no coercion to numbers, in this case
                                (is_string? (term v))
                                (equal? (term unop) (term \#)))
                            
                               (equal? (term unop) (term not))))]
   
   ; equality comparison
   [-->s/e (v_1 == v_2)
           true
           E-EqualitySuccess
           (side-condition (is_true? (term (δ == v_1 v_2))))]

   [-->s/e (v_1 == v_2)
           false
           E-EqualityFailNtables
           (side-condition (is_false? (term (δ == v_1 v_2))))
           (side-condition (or (not (is_tid? (term v_1)))
                               (not (is_tid? (term v_2)))))]

   ; rel ops
   ; translation of expressions involving > and >=
   [-->s/e (v_1 binop v_2)
           (v_2 any v_1)
           E-TranslateComparison
        
           (side-condition (or (equal? (term binop) (term >))
                               (equal? (term binop) (term >=))))
           (where any (translateComparisonOp binop))]
   
   [-->s/e (v_1 binop v_2)
           (δ binop v_1 v_2)
           E-RelOp

           ; relational op (< or <=) over numeric or string operands
           (side-condition (and (term (isRelationalOperator binop))

                                (equal? (term (δ type v_1))
                                        (term (δ type v_2)))

                                (or (is_string? (term v_1))
                                    (is_number? (term v_1)))))]

   ; abnormal situations with primitive operators
   [-->s/e (v_1 binop v_2)
           ((v_1 binop v_2)ArithWrongOps)
           E-ArithBinOpWrongOps

           ; arith op over non-numeric or non-string operands
           (side-condition (term (isArithBinOp binop)))

           ; the operation was not successful
           (where any (δ binop v_1 v_2))

           (side-condition (not (is_number? (term any))))

           ]
   
   [-->s/e (- v)
           ((- v)NegWrongOp)
           E-AlertNegationWrongOperand

           ; the operation was not successful
           (where any (δ - v))

           (side-condition (not (is_number? (term any))))]
   
   [-->s/e (v_1 .. v_2)
           ((v_1 .. v_2)StrConcatWrongOps)
           E-AlertStringConcatWrongOperands

           ; concat only fails when applied to operands of unexpected type
           (where any (δ .. v_1 v_2))

           (side-condition (not (is_string? (term any))))]
   
   [-->s/e (\# v)
           ((\# v)StrLenWrongOp)
           E-AlertStringLengthWrongOperand

           ; no coercion here
           (side-condition (not (is_string? (term v))))]
   
   [-->s/e (v_1 == v_2)
           ((v_1 == v_2)EqFail)
        
           E-AlertEqualityFail
        
           (side-condition (is_false? (term (δ == v_1 v_2))))
           (side-condition (and (is_tid? (term v_1))
                                (is_tid? (term v_2))))]
   
   [-->s/e (v_1 binop v_2)
           ((v_1 binop v_2)OrdCompWrongOps)
           E-AlertOrdCompWrongOps
        
           (side-condition (and (term (isRelationalOperator binop))
                             
                                (not (and (equal? (term (δ type v_1))
                                                  (term (δ type v_2)))
                                       
                                          (or (is_string? (term v_1))
                                              (is_number? (term v_1)))))))
           ]


   ; built-in services
   [-->s/e ($builtIn builtinserv (v ...))
           (δ builtinserv v ...)

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
   [-->s/e (if v then scoreblock_1 else scoreblock_2 end)
           scoreblock_1
           IfTrue
      
           (side-condition (not (is_false_cond? (term v))))]
   
   [-->s/e (if v then scoreblock_1 else scoreblock_2 end)
           scoreblock_2
           IfFalse
      
           (side-condition (is_false_cond? (term v)))]

   ; While statement
   [-->s/e (while e do scoreblock end)
           (($iter e do scoreblock end) Break)
           SignpostWhile]
   
   [-->s/e ($iter e do scoresing end)
           (if e then (scoresing ($iter e do scoresing end)) else \; end)
           While_single_stat]

   [-->s/e ($iter e do (scoresing_1 scoresing_2 scoresing_3 ...) end)
           (if e then
               (scoresing_1 scoresing_2 scoresing_3 ...
                        ($iter e do (scoresing_1 scoresing_2 scoresing_3 ...) end))
               else \; end)
           While_conc_stats]

   
   ; concatenation of statements
   ; this added rule has to do with the concrete grammar used
   ; in this mechanization.
   [-->s/e (\; scoresing)
           scoresing
           ConcatBehavior]

   [-->s/e (\; scoresing_1 scoresing_2 scoresing_3 ...)
           (scoresing_1 scoresing_2 scoresing_3 ...)
           ConcatBehavior2]

   ; Do ... End block
   [-->s/e (do \; end)
           \;
           DoEnd]

   ; list length-equating rules for assignment statements
   ; the rule only make sense when there are 2 or more r-values (spec. useful
   ; for redex-check'ing purposes)
   [-->s/e (evar_1 evar_2 ..._1 = v_1 v_2 ..._1 v_3 v_4 ...)
           (evar_1 evar_2 ... = v_1 v_2 ...)
           
           AssignDiscardRvalues]
   
   [-->s/e (evar_1 ..._1 evar_2 evar_3 ..._2 = v_1 ..._1)
           (evar_1 ... evar_2 evar_3 ... = v_1 ... nil nil ..._2)
           
           Assgn-Fewer]
   
   ; same length on both sides of = 
   [-->s/e (evar_1 evar_2 ..._1 evar_3 = v_1 v_2 ..._1 v_3)
           ((evar_3 = v_3) (evar_1 evar_2 ... = v_1 v_2 ...))
           AssignSplit]

   [-->s/e (local Name_1 Name_2 ..._1 = v_1 v_2 ..._1 v_3 v_4 ... in scoreblock end)
           (local Name_1 Name_2 ... = v_1 v_2 ... in scoreblock end)
           
           LocalDiscardRvalues]

   
   [-->s/e (local Name_1 ..._1 Name_2 Name_3 ..._2 = v_1 ..._1 in scoreblock end)
           (local Name_1 ... Name_2 Name_3 ... = v_1 ... nil nil ..._2 in scoreblock end)
        
           LocalCompleteRvalues]

   ; Break
   [-->s/e ((in-hole Elf break) Break)
           \;
           Break]

   [-->s/e (\; Break)
           \;
           FinalizationWhile]

   ; call over a non-function value (both, function call as stat and exp)
   [-->s/e ($statFunCall ..._1 v (v_1 ...))
           (($statFunCall ..._1 v (v_1 ...)) WFunCall)

           E-AlertWrongStatFunCall
           ; Determine that v_1 is not a reference to a function
           (side-condition (not (is_cid? (term v))))]
   
   ; method call (both, method call as stat and exp)
   [-->s/e ($statFunCall ..._1 v : Name (e ...))
           ($statFunCall ..._1 (v \[ String \]) (v e ...))
           E-MethodCallStat

           (where String ,(symbol->string (term Name)))]

   ; Return
   [-->s/e ((in-hole Elf (return v ...)) (renv ...) RetStat)
           \;
           E-DiscardValues]

   [-->s/e (\; (renv ...) RetStat)
           \;
           E-ReturnNoValues]

   [-->s/e ((in-hole Elf (return v ...)) (renv ...) RetExp)
           (< v ... >)
           E-ReturnValues]

   [-->s/e (\; (renv ...) RetExp)
           (< >)
           E-ReturnEmptyTuple]

   
   [-->s/e ((in-hole Elf (return v ...)) Break)
           (return v ...)
           E-InteractionReturnBreak]

   ; loc. variables
   [-->s/e (\; (renv ...) LocalBody)
           \;
           E-RemoveLocal]
   
   ; Protected mode
   [-->s/e ((in-hole Enp ($err v_1)) ProtectedMode v_2)
           (< false "error in error handling" >)
        
           E-XProtectedModeErrorCatchedNoHandler

           (side-condition (not (is_cid? (term v_2))))]

   [-->s/e ((in-hole Enp ($err v)) ProtectedMode cid)
           (cid (v))
        
           E-XProtectedModeErrorCatchedHandler]

   [-->s/e ((< v_1 ... >) ProtectedMode v_2)
           (< true (< v_1 ... >) >)
        
           E-XProtectedModeNoErrorWithReturnedValues]
   ))

(provide terms-rel)
