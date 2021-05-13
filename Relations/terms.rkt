#lang racket
; Expressions that don't interact with some store

(require redex
         "../grammar.rkt"
         "../Meta-functions/grammarMetaFunctions.rkt"
         "../Meta-functions/delta.rkt")

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
   
   [-->s/e (in-hole Ea (< v_1 v_2 ... >))
           (fix_unwrap Ea (v_1 v_2 ...))
           E-UnwrapNonEmptyTuple]

   [-->s/e (in-hole Ea (< >))
           (fix_unwrap Ea '())
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
        
           ; apply binop, check if the operation was successful
           (where v_3 (δ binop v_1 v_2))

           ; we are assuming the soundness of δ: successful arithops only return
           ; numbers, successful concat only returns strings, etc
           ; if (δ binop v_1 v_2) is a value (i.e. not an error), the operation
           ; was successful
           (side-condition (or (is_strconcat? (term binop))
                               
                               (is_arithop? (term binop))
                               
                               (is_lt_le? (term binop))))
           E-BinOp]
   
   ; logical conectives
   [-->s/e (v binop e)
           (δ binop v e)
           
           (side-condition (is_bool_binop? (term binop)))

           E-LogicOp]
   
   ; unary operations
   [-->s/e (- v)
           Number
        
           ; check if the operation was successful
           (where Number (δ - v))

           E-NumNeg]
   
   [-->s/e (unop v)
           (δ unop v)
           
           (side-condition (or (and ; there is no coercion to numbers, in this case
                                (is_string? (term v))
                                (equal? (term unop) (term \#)))
                            
                               (equal? (term unop) (term not))))

           E-UnOp]
   
   ; equality comparison
   [-->s/e (v_1 == v_2)
           true
           
           (side-condition (is_true? (term (δ == v_1 v_2))))

           E-EqualitySuccess]

   [-->s/e (v_1 == v_2)
           false
           
           (side-condition (is_false? (term (δ == v_1 v_2))))
           (side-condition (or (not (is_tid? (term v_1)))
                               (not (is_tid? (term v_2)))))

           E-EqualityFailNtables]

   ; rel ops
   ; translation of expressions involving > and >=
   [-->s/e (v_1 binop v_2)
           (v_2 any v_1)
           
           (side-condition (or (equal? (term binop) (term >))
                               (equal? (term binop) (term >=))))
           
           (where any (trans_comp_op binop))

           E-TranslateComparison]

   ; abnormal situations with primitive operators
   [-->s/e (v_1 binop v_2)
           ((v_1 binop v_2) BinopWO)
           
           (where e (δ binop v_1 v_2))

           (side-condition (and (or (is_strconcat? (term binop))
                                    (is_arithop? (term binop))
                                    (is_lt_le? (term binop)))
                               
                                (not (is_v? (term e)))))

           Binop-Wo]
   
   [-->s/e (- v)
           ((- v) NegWrongOp)
           E-AlertNegationWrongOperand

           ; the operation was not successful
           (where any (δ - v))

           (side-condition (not (is_number? (term any))))]
   
   [-->s/e (\# v)
           ((\# v) StrLenWrongOp)
           E-AlertStringLengthWrongOperand

           ; no coercion here
           (side-condition (not (is_string? (term v))))]
   
   [-->s/e (v_1 == v_2)
           ((v_1 == v_2) EqFail)
        
           E-AlertEqualityFail
        
           (side-condition (is_false? (term (δ == v_1 v_2))))
           (side-condition (and (is_tid? (term v_1))
                                (is_tid? (term v_2))))]


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
                                          table.concat
                                          table.insert
                                          table.pack
                                          ))))
           E-BuiltInTerm]

   ; protected mode
   ; no error
   [-->s/e ((< v_1 ... >) ProtMD v_2)
           (< true v_1 ... >)
        
           E-ProtTrue]
   
   ; error, but with a proper handler
   [-->s/e ((in-hole Enp ($err v)) ProtMD cid)
           ((\( (cid (v)) \)) ProtMD)
        
           E-ProtHandler]

   ; error without a proper handler, or not a handler at all
   [-->s/e ((in-hole Enp ($err v_1)) ProtMD v_2 ...)
           (< false "error in error handling" >)
        
           (side-condition (not (redex-match? ext-lang
                                              (cid)
                                              (term (v_2 ...)))))

           E-ProtHandlerErr]

   ; no error during error handling
   [-->s/e (v ProtMD)
           (< false v >)
           
           E-ProtFalse]
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
   [-->s/e (if v then s_1 else s_2 end)
           s_1
           If-T
      
           (side-condition (not (is_false_cond? (term v))))]
   
   [-->s/e (if v then s_1 else s_2 end)
           s_2
           If-F
      
           (side-condition (is_false_cond? (term v)))]

   ; While statement
   [-->s/e (while e do s end)
           (($iter e do s end) Break)
           
           While-Start]
   
   [-->s/e ($iter e do sing end)
           (if e then (sing ($iter e do sing end)) else \; end)
           
           While-Iter-Sing]

   [-->s/e ($iter e do (sing_1 sing_2 sing_3 ...) end)
           (if e then
               (sing_1 sing_2 sing_3 ...
                        ($iter e do (sing_1 sing_2 sing_3 ...)
                               end))
               else \; end)
           
           While-Iter-Concat]

   
   ; concatenation of statements
   ; this added rule has to do with the concrete grammar used
   ; in this mechanization.
   [-->s/e (\; sing)
           sing

           Seq-Single]

   [-->s/e (\; sing_1 sing_2 sing_3 ...)
           (sing_1 sing_2 sing_3 ...)
           
           Seq-Concat]

   ; Do ... End block
   [-->s/e (do \; end)
           \;
           
           DoEnd]

   ; list length-equating rules for assignment statements
   ; the rule only make sense when there are 2 or more r-values (spec. useful
   ; for redex-check'ing purposes)
   [-->s/e (evar_1 evar_2 ..._1 = v_1 v_2 ..._1 v_3 v_4 ...)
           (evar_1 evar_2 ... = v_1 v_2 ...)
           
           Assgn-More]
   
   [-->s/e (evar_1 ..._1 evar_2 evar_3 ..._2 = v_1 ..._1)
           (evar_1 ... evar_2 evar_3 ... = v_1 ... nil nil ..._2)
           
           Assgn-Fewer]
   
   ; same length on both sides of = 
   [-->s/e (evar_1 evar_2 ..._1 evar_3 = v_1 v_2 ..._1 v_3)
           ((evar_3 = v_3) (evar_1 evar_2 ... = v_1 v_2 ...))

           Assgn-Split]

   [-->s/e (local Name_1 Name_2 ..._1 = v_1 v_2 ..._1 v_3 v_4 ... in s end)
           (local Name_1 Name_2 ... = v_1 v_2 ... in s end)
           
           LocalDiscardRvalues]

   
   [-->s/e (local Name_1 ..._1 Name_2 Name_3 ..._2 = v_1 ..._1 in s end)
           (local Name_1 ... Name_2 Name_3 ... = v_1 ... nil nil ..._2 in s end)
        
           LocalCompleteRvalues]

   ; Break
   [-->s/e ((in-hole Elf break) Break)
           \;
           
           While-Break]

   [-->s/e (\; Break)
           \;
           
           While-End]

   ; call over a non-function value (both, function call as stat and exp)
   [-->s/e ($statFCall ..._1 v (v_1 ...))
           (($statFCall ..._1 v (v_1 ...)) WFunCall)

           E-AlertWrongStatFunCall
           ; Determine that v_1 is not a reference to a function
           (side-condition (not (is_cid? (term v))))]
   
   ; method call (both, method call as stat and exp)
   [-->s/e ($statFCall ..._1 v : Name (e ...))
           ($statFCall ..._1 (v \[ String \]) (v e ...))
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
   ))

(provide terms-rel)
