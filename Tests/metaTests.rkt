#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/meta.rkt")

(define (meta-test-suite)
  ; Function call
  ; E-WFunCallWithHandler
  (test-->> meta
            (term ((((cl 2) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1) ((\{ (\[ "__call" \] = (cl 2)) \}) nil 0))) 
                   : ((1 (2)) WFunCall)))
            
            (term ((((cl 2) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1) ((\{ (\[ "__call" \] = (cl 2)) \}) nil 0))) 
                   : (((cl 2) (1 2)) Meta (objr 1)))))

  ; E-WFunCallNoHandler
  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ((1 (2)) WFunCall)))
            
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ($err "attempt to call a number value."))))
  
  (test-->> meta
            (term (() : ((1 (2))WFunCall)))
            
            (term (() : ($err "attempt to call a number value."))))
  
  ; E-WrongKeyWithHandlerNormal
  (test-->> meta
            (term ((((objr 1) 
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((cl 3) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 2) 
                     ((\{ (\[ "__index" \] = (cl 3)) \}) nil 0))) 
                   : (((objr 1) \[ 2 \]) WrongKey)))
            
            (term ((((objr 1) 
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((cl 3) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 2) 
                     ((\{ (\[ "__index" \] = (cl 3)) \}) nil 0))) 
                   : ((\( ((cl 3) ((objr 1) 2)) \)) Meta (objr 2)))))
  
  ; E-WrongKeyWithHandlerRepeat
  (test-->> meta
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = 1) \}) nil 0))) 
                   : (((objr 1) \[ 2 \]) WrongKey)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = 1) \}) nil 0))) 
                   : ((1 |[| 2 |]|) Meta (objr 2)))))
  
  (test-->> meta
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = (objr 3)) \}) nil 0))
                    ((objr 3)
                     ((\{  \}) nil 0))) 
                   : (((objr 1) \[ 2 \]) WrongKey)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = (objr 3)) \}) nil 0))
                    ((objr 3)
                     ((\{  \}) nil 0)))
                   : (((objr 3) \[ 2 \]) Meta (objr 2)))))

  ; E-WrongKeyNoHandler
  (test-->> meta
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) nil 0))) 
                   : (((objr 1) \[ 2 \]) WrongKey)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) nil 0)))
                   : nil)))
  
  (test-->> meta
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((objr 2)
                     ((\{ \}) nil 0))) 
                   : (((objr 1) \[ 2 \]) WrongKey)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))
                    ((objr 2)
                     ((\{ \}) nil 0)))
                   : nil)))
  
  ; E-NonTableIndexedWithHandlerNormal
  (test-->> meta
            (term ((((cl 2) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1)
                     ((\{ (\[ "__index" \] = (cl 2)) \}) nil 0))) 
                   : ((1 \[ 2 \]) NonTable)))
            
            (term ((((cl 2) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1)
                     ((\{ (\[ "__index" \] = (cl 2)) \}) nil 0))) 
                   : ((\( ((cl 2) (1 2)) \)) Meta (objr 1)))))
  
  ; E-NonTableIndexedWithHandlerRepeat
  (test-->> meta
            (term ((((objr 1)
                     ((\{ (\[ "__index" \] = (objr 6)) \}) nil 0))
                    ((objr 6)
                     ((\{ \}) nil 0))) 
                   : ((1 \[ 2 \] )NonTable)))
            
            (term ((((objr 1)
                     ((\{ (\[ "__index" \] = (objr 6)) \}) nil 0))
                    ((objr 6)
                     ((\{ \}) nil 0)))
                   : (((objr 6) \[ 2 \]) Meta (objr 1)))))
  
  ; E-NonTableIndexedNoHandler
  (test-->> meta
            (term (() : ((1 \[ 2 \])NonTable)))
            (term (() : ($err "attempt to index a number value."))))
  
  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ((1 \[ 2 \])NonTable)))
            
            (term ((((objr 1) ((\{ \}) nil 0)))
                   : ($err "attempt to index a number value."))))
  
  ; E-AdditionWrongOperandsWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__add" \] = 1) \}) nil 0))) 
                   : (("q" + "q") BinopWO)))
            (term ((((objr 4) ((\{ (\[ "__add" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q" "q")) \)) Meta (objr 4)))))
  
  ; E-AdditionWrongOperandsNoHandler
  (test-->> meta
            (term (() : (("q" + "q")BinopWO)))
            (term (() : ($err
                         "attempt to perform arithmetic on a string value."))))
  
  ; E-SubstractionWrongOperandsWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__sub" \] = 1) \}) nil 0))) 
                   : (("q" - "q") BinopWO)))
            (term ((((objr 4) ((\{ (\[ "__sub" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q" "q")) \)) Meta (objr 4)))))
  
  ; E-SubstractionWrongOperandsNoHandler
  (test-->> meta
            (term (() : (("q" - "q")BinopWO)))
            (term (() : ($err "attempt to perform arithmetic on a string value."))))
  
  ; E-MultiplicationWrongOperandsWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__mul" \] = 1) \}) nil 0))) 
                   : (("q" * "q") BinopWO)))
            (term ((((objr 4) ((\{ (\[ "__mul" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q" "q")) \)) Meta (objr 4)))))
  
  ; E-MultiplicationWrongOperandsNoHandler
  (test-->> meta
            (term (() : (("q" * "q")BinopWO)))
            (term (() : ($err "attempt to perform arithmetic on a string value."))))
  
  ; E-DivisionWrongOperandsWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__div" \] = 1) \}) nil 0))) 
                   : (("q" / "q")BinopWO)))
            (term ((((objr 4) ((\{ (\[ "__div" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q" "q")) \)) Meta (objr 4)))))
  
  ; E-DivisionWrongOperandsNoHandler
  (test-->> meta
            (term (() : (("q" / "q")BinopWO)))
            (term (() : ($err "attempt to perform arithmetic on a string value."))))
  
  ; E-ExponentiationWrongOperandsWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__pow" \] = 1) \}) nil 0))) 
                   : (("q" ^ "q")BinopWO)))
            (term ((((objr 4) ((\{ (\[ "__pow" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q" "q")) \)) Meta (objr 4)))))
  
  ; E-ExponentiationWrongOperandsNoHandler
  (test-->> meta
            (term (() : (("q" ^ "q")BinopWO)))
            (term (() : ($err "attempt to perform arithmetic on a string value."))))
  
  ; E-ModuleWrongOperandsWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__mod" \] = 1) \}) nil 0))) 
                   : (("q" % "q")BinopWO)))
            (term ((((objr 4) ((\{ (\[ "__mod" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q" "q")) \)) Meta (objr 4)))))
  
  ; E-ModuleWrongOperandsNoHandler
  (test-->> meta
            (term (() : (("q" % "q")BinopWO)))
            (term
             (()
              :
              ($err "attempt to perform arithmetic on a string value."))))
  
  ; E-NegationWrongOperandWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__unm" \] = 1) \}) nil 0))) 
                   : ((- "q")NegWrongOp)))
            (term ((((objr 4) ((\{ (\[ "__unm" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q")) \)) Meta (objr 4) ))))
  
  ; E-NegationWrongOperandsNoHandler
  (test-->> meta
            (term (() : ((- "q")NegWrongOp)))
            (term
             (()
              :
              ($err "attempt to perform arithmetic on a string value."))))
  
  ; E-StringConcatWrongOperandsWithHandler
  (test-->> meta
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil 0))) 
                   : ((1 .. "q") BinopWO)))
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil 0))) 
                   : ((\( (1 (1 "q")) \)) Meta (objr 1)))))
  
  (test-->> meta
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil 0))) 
                   : (("q" .. 1)BinopWO)))
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil 0))) 
                   : ((\( (1 ("q" 1)) \)) Meta (objr 1)))))
  
  ; E-StringConcatWrongOperandsWithHandler
  (test-->> meta
            (term (() : ((1 .. "q") BinopWO)))
            (term
             (()
              :
              ($err "attempt to concatenate a number value."))))
  
  ; E-StringLengthWrongOperandWithHandler
  (test-->> meta
            (term ((((objr 1) ((\{ (\[ "__len" \] = 1) \}) nil 0))) 
                   : ((\# 1) StrLenWrongOp)))
            (term ((((objr 1) ((\{ (\[ "__len" \] = 1) \}) nil 0))) 
                   : ((\( (1 (1)) \)) Meta (objr 1)))))
  
  ; E-StringLengthWrongOperandTableLength
  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ((\# (objr 1)) StrLenWrongOp)))
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : 0)))
  
  (test-->> meta
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ "a" \] = 1)
                                   (\[ 2 \] = 1) \}) nil 0))) 
                   : ((\# (objr 1)) StrLenWrongOp)))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ "a" \] = 1)
                                   (\[ 2 \] = 1) \}) nil 0))) 
                   : 2)))
  
  ; E-StringLengthWrongOperandNoHandler
  (test-->> meta
            (term (() 
                   : ((\# 1) StrLenWrongOp)))
            (term
             (() 
              :
              ($err "attempt to get length of a number value."))))
  
  ; E-EqualityFailWithHandler
  (test-->> meta
            (term (() 
                   : ((1 == 2)EqFail)))
            (term (() 
                   : false)))
  
  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0)) 
                    ((objr 2) ((\{ \}) nil 0))) 
                   : (((objr 1) == (objr 2))EqFail)))
            
            (term ((((objr 1) ((\{ \}) nil 0)) 
                    ((objr 2) ((\{ \}) nil 0))) 
                   : false)))
  
  (test-->> meta
            (term ((((objr 1) ((\{ \}) (objr 3) 0)) 
                    ((objr 2) ((\{ \}) (objr 3) 0))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil 0))) 
                   : (((objr 1) == (objr 2))EqFail)))
            
            (term ((((objr 1) ((\{ \}) (objr 3) 0)) 
                    ((objr 2) ((\{ \}) (objr 3) 0))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil 0))) 
                   : ((not (not (1 ((objr 1) (objr 2))))) Meta (objr 3)))))
  
  ; E-EqualityFailNoHandler
  (test-->> meta
            (term ((((objr 1) ((\{ \}) (objr 3) 0)) 
                    ((objr 2) ((\{ \}) nil 0))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil 0))) 
                   : (((objr 1) == (objr 2))EqFail)))
            
            (term ((((objr 1) ((\{ \}) (objr 3) 0)) 
                    ((objr 2) ((\{ \}) nil 0))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil 0))) 
                   : false)))
  
  ; E-LessThanFailWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__lt" \] = 1) \}) nil 0))) 
                   : (("a" < 1) BinopWO)))
            (term ((((objr 4) ((\{ (\[ "__lt" \] = 1) \}) nil 0))) 
                   : ((not (not (1 ("a" 1)))) Meta (objr 4)))))
  
  ; E-LessThanFailNoHandler
  (test-->> meta
            (term (() : (("a" < 1) BinopWO)))
            (term
             (()
              :
              ($err "attempt to compare string with number"))))
  
  ; E-LessThanOrEqualFailWithHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__le" \] = 1) \}) nil 0))) 
                   : (("a" <= 1) BinopWO)))
            
            (term ((((objr 4) ((\{ (\[ "__le" \] = 1) \}) nil 0))) 
                   : ((not (not (1 ("a" 1)))) Meta (objr 4)))))
  
  ; E-LessThanOrEqualFailWithAltHandler
  (test-->> meta
            (term ((((objr 4) ((\{ (\[ "__le" \] = nil)
                                   (\[ "__lt" \] = 1) \}) nil 0))) 
                   : (("a" <= 1) BinopWO)))
            
            (term ((((objr 4) ((\{ (\[ "__le" \] = nil)
                                   (\[ "__lt" \] = 1) \}) nil 0))) 
                   : ((not (1 (1 "a"))) Meta (objr 4)))))
  
  ; E-LessThanOrEqualFailNoHandler
  (test-->> meta
            (term (() : (("a" <= 1) BinopWO)))
            (term
             (()
              :
              ($err "attempt to compare string with number"))))

  
;                                                                                  
;                                                                                  
;                                                                                  
;    ;;;;;    ;               ;                                       ;            
;   ;;    ;   ;               ;                                       ;            
;   ;       ;;;;;;    ;;;   ;;;;;;   ;;;;   ;;;;;;;  ;;;;   ; ;;;   ;;;;;;   ;;;;  
;   ;;        ;      ;   ;    ;     ;;  ;;  ;  ;  ; ;;  ;;  ;;   ;    ;     ;    ; 
;    ;;;;;    ;          ;    ;     ;    ;  ;  ;  ; ;    ;  ;    ;    ;     ;      
;        ;;   ;      ;;;;;    ;     ;;;;;;  ;  ;  ; ;;;;;;  ;    ;    ;      ;;;;  
;         ;   ;     ;    ;    ;     ;       ;  ;  ; ;       ;    ;    ;          ; 
;   ;    ;;   ;     ;   ;;    ;     ;;   ;  ;  ;  ; ;;   ;  ;    ;    ;     ;    ; 
;    ;;;;;     ;;;   ;;; ;     ;;;   ;;;;   ;  ;  ;  ;;;;   ;    ;     ;;;   ;;;;  
;                                                                                  
;                                                                                  
;                                                                                  
; E-FieldAssignWrongKeyNormal
  (test-->> meta
            (term ((((cl 3) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1) ((\{ (\[ "__newindex" \] = (cl 3)) \}) nil 0))
                    ((objr 2) ((\{ \}) (objr 1) 0))) 
                   : ((((objr 2) \[ 1 \]) = 2) WrongKey)))
            
            (term ((((cl 3) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1) ((\{ (\[ "__newindex" \] = (cl 3)) \}) nil 0))
                    ((objr 2) ((\{ \}) (objr 1) 0))) 
                   : (($statFunCall (cl 3) ((objr 2) 1 2)) Meta (objr 1)))))
  
  ; E-FieldAssignWrongKeyRepeat
  (test-->> meta
            (term ((((objr 1)
                     ((\{ (\[ "__newindex" \] = (objr 3)) \}) nil 0))
                    ((objr 2)
                     ((\{ \}) (objr 1) 0))
                    ((objr 3)
                     ((\{ (\[ 1 \] = 1) \}) nil 0)))
                   : ((((objr 2) \[ 1 \]) = 2) WrongKey)))
            
            (term ((((objr 1)
                     ((\{ (\[ "__newindex" \] = (objr 3)) \}) nil 0))
                    ((objr 2)
                     ((\{ \}) (objr 1) 0))
                    ((objr 3)
                     ((\{ (\[ 1 \] = 1) \}) nil 0))) 
                   : ((((objr 3) \[ 1 \]) = 2) Meta (objr 1)))))
  
  ; E-FieldAssignWrongKeyNoHandler
  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0))
                    ((objr 2) ((\{ \}) (objr 1) 0))) 
                   : ((((objr 2) \[ 1 \]) = 2)WrongKey)))
            
            (term ((((objr 1) ((\{ \}) nil 0))
                    ((objr 2) ((\{ (\[ 1 \] = 2) \}) (objr 1) 0))) 
                   : \;)))
  
  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0)))
                   : ((((objr 1) \[ 1 \]) = 2)WrongKey)))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : \;)))

  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0)))
                   : ((((objr 1) \[ nil \]) = 2)WrongKey)))
            
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ($err "table index is nil"))))

  (test-->> meta
            (term ((((objr 1) ((\{ \}) nil 0)))
                   : ((((objr 1) \[ +nan.0 \]) = 2)WrongKey)))
            
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ($err "table index is NaN"))))
  
  ; E-FieldAssignOverNonTableNormal
  (test-->> meta
            (term ((((cl 2) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1) 
                     ((\{ (\[ "__newindex" \] = (cl 2)) \}) nil 0)))
                   : (((1 \[ 2 \]) = 2) NonTable)))
            
            (term ((((cl 2) (function X () ($statFunCall (ref 1) ()) end))
                    ((objr 1) 
                     ((\{ (\[ "__newindex" \] = (cl 2)) \}) nil 0)))
                   : (($statFunCall (cl 2) (1 2 2)) Meta (objr 1)))))
  
    ; E-FieldAssignOverNonTableRepeat
    (test-->> meta
              (term ((((objr 1)
                       ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil 0))
                      ((objr 6)
                       ((\{ \}) nil 0)))
                     : (((1 \[ 2 \]) = 3) NonTable)))
              
              (term ((((objr 1)
                       ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil 0))
                      ((objr 6)
                       ((\{ \}) nil 0)))
                     : ((((objr 6) \[ 2 \]) = 3) Meta (objr 1)))))
    
    ; E-FieldAssignOverNonTableNoHandler
    (test-->> meta
              (term (() : (((1 \[ 2 \]) = 3)NonTable)))
              (term (() : ($err "attempt to index a number value")))

              )
    
    (test-->> meta
              (term ((((objr 1) ((\{ \}) nil 0)))
                     : (((1 \[ 2 \]) = 3)NonTable)))


              (term ((((objr 1) ((\{ \}) nil 0)))
                     : ($err "attempt to index a number value")))

              )


  (test-results))

(provide meta-test-suite)
