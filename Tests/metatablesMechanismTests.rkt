#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/metatablesMechanism.rkt")

(define (metatables-mechanism-test-suite)
  ; Function call
  ; E-WrongFunCallWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ "__call" \] = (objr 2)) \}) nil))
                    ((objr 2)
                     (function X () ((ref 1) ()) end))) 
                   : ((1 (2))WrongFunCall)))
            
            (term ((((objr 1)
                     ((\{ (\[ "__call" \] = (objr 2)) \}) nil))
                    ((objr 2)
                     (function X () ((ref 1) ()) end))) 
                   : ((objr 2) (1 2)))))

  ; E-WrongFunCallNoHandler
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ((1 (2))WrongFunCall)))
            
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ($builtIn error ("attempt to call a number value.")))))
  
  (test-->> metatables-mechanism
            (term (() : ((1 (2))WrongFunCall)))
            
            (term (() : ($builtIn error ("attempt to call a number value.")))))
  
  ; E-KeyNotFoundWithHandlerNormal
  (test-->> metatables-mechanism
            (term ((((objr 1) 
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2) 
                     ((\{ (\[ "__index" \] = (objr 3)) \}) nil))
                    ((objr 3) 
                     (function X () ((ref 1) ()) end))) 
                   : (((objr 1) \[ 2 \])KeyNotFound)))
            
            (term ((((objr 1) 
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2) 
                     ((\{ (\[ "__index" \] = (objr 3)) \}) nil))
                    ((objr 3) 
                     (function X () ((ref 1) ()) end))) 
                   : ((objr 3) ((objr 1) 2)))))
  
  ; E-KeyNotFoundWithHandlerRepeat
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = 1) \}) nil))) 
                   : (((objr 1) \[ 2 \])KeyNotFound)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = 1) \}) nil))) 
                   : (1 \[ 2 \]))))
  
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = (objr 3)) \}) nil))
                    ((objr 3)
                     ((\{  \}) nil))) 
                   : (((objr 1) \[ 2 \])KeyNotFound)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2)
                     ((\{ (\[ "__index" \] = (objr 3)) \}) nil))
                    ((objr 3)
                     ((\{  \}) nil)))
                   : ((objr 3) \[ 2 \]))))

  ; E-KeyNotFoundNoHandler
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) nil))) 
                   : (((objr 1) \[ 2 \])KeyNotFound)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) nil)))
                   : nil)))
  
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2)
                     ((\{ \}) nil))) 
                   : (((objr 1) \[ 2 \])KeyNotFound)))
            
            (term ((((objr 1)
                     ((\{ (\[ 1 \] = 1) \}) (objr 2)))
                    ((objr 2)
                     ((\{ \}) nil)))
                   : nil)))
  
  ; E-NonTableIndexedWithHandlerNormal
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ "__index" \] = (objr 6)) \}) nil))
                    ((objr 6)
                     (function X () ((ref 1) ()) end))) 
                   : ((1 \[ 2 \])NonTableIndexed)))
            
            (term ((((objr 1)
                     ((\{ (\[ "__index" \] = (objr 6)) \}) nil))
                    ((objr 6)
                     (function X () ((ref 1) ()) end))) 
                   : ((objr 6) (1 2)))))
  
  ; E-NonTableIndexedWithHandlerRepeat
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ "__index" \] = (objr 6)) \}) nil))
                    ((objr 6)
                     ((\{ \}) nil))) 
                   : ((1 \[ 2 \])NonTableIndexed)))
            
            (term ((((objr 1)
                     ((\{ (\[ "__index" \] = (objr 6)) \}) nil))
                    ((objr 6)
                     ((\{ \}) nil)))
                   : ((objr 6) \[ 2 \]))))
  
  ; E-NonTableIndexedNoHandler
  (test-->> metatables-mechanism
            (term (() : ((1 \[ 2 \])NonTableIndexed)))
            (term (() : ($builtIn error ("attempt to index a number value.")))))
  
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ((1 \[ 2 \])NonTableIndexed)))
            
            (term ((((objr 1) ((\{ \}) nil)))
                   : ($builtIn error ("attempt to index a number value.")))))
  
  ; E-AdditionWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__add" \] = 1) \}) nil))) 
                   : (("q" + "q")ArithWrongOps)))
            (term ((((objr 4) ((\{ (\[ "__add" \] = 1) \}) nil))) 
                   : (1 ("q" "q")))))
  
  ; E-AdditionWrongOperandsNoHandler
  (test-->> metatables-mechanism
            (term (() : (("q" + "q")ArithWrongOps)))
            (term (() : ($builtIn error
                                   ("attempt to perform arithmetic on a string value.")))))
  
  ; E-SubstractionWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__sub" \] = 1) \}) nil))) 
                   : (("q" - "q")ArithWrongOps)))
            (term ((((objr 4) ((\{ (\[ "__sub" \] = 1) \}) nil))) 
                   : (1 ("q" "q")))))
  
  ; E-SubstractionWrongOperandsNoHandler
  (test-->> metatables-mechanism
            (term (() : (("q" - "q")ArithWrongOps)))
            (term (() : ($builtIn error
                                   ("attempt to perform arithmetic on a string value.")))))
  
  ; E-MultiplicationWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__mul" \] = 1) \}) nil))) 
                   : (("q" * "q")ArithWrongOps)))
            (term ((((objr 4) ((\{ (\[ "__mul" \] = 1) \}) nil))) 
                   : (1 ("q" "q")))))
  
  ; E-MultiplicationWrongOperandsNoHandler
  (test-->> metatables-mechanism
            (term (() : (("q" * "q")ArithWrongOps)))
            (term (() : ($builtIn error
                                   ("attempt to perform arithmetic on a string value.")))))
  
  ; E-DivisionWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__div" \] = 1) \}) nil))) 
                   : (("q" / "q")ArithWrongOps)))
            (term ((((objr 4) ((\{ (\[ "__div" \] = 1) \}) nil))) 
                   : (1 ("q" "q")))))
  
  ; E-DivisionWrongOperandsNoHandler
  (test-->> metatables-mechanism
            (term (() : (("q" / "q")ArithWrongOps)))
            (term (() : ($builtIn error
                                   ("attempt to perform arithmetic on a string value.")))))
  
  ; E-ExponentiationWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__pow" \] = 1) \}) nil))) 
                   : (("q" ^ "q")ArithWrongOps)))
            (term ((((objr 4) ((\{ (\[ "__pow" \] = 1) \}) nil))) 
                   : (1 ("q" "q")))))
  
  ; E-ExponentiationWrongOperandsNoHandler
  (test-->> metatables-mechanism
            (term (() : (("q" ^ "q")ArithWrongOps)))
            (term (() : ($builtIn error
                                   ("attempt to perform arithmetic on a string value.")))))
  
  ; E-ModuleWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__mod" \] = 1) \}) nil))) 
                   : (("q" % "q")ArithWrongOps)))
            (term ((((objr 4) ((\{ (\[ "__mod" \] = 1) \}) nil))) 
                   : (1 ("q" "q")))))
  
  ; E-ModuleWrongOperandsNoHandler
  (test-->> metatables-mechanism
            (term (() : (("q" % "q")ArithWrongOps)))
            (term (() : ($builtIn error
                                   ("attempt to perform arithmetic on a string value.")))))
  
  ; E-NegationWrongOperandWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__unm" \] = 1) \}) nil))) 
                   : ((- "q")NegWrongOp)))
            (term ((((objr 4) ((\{ (\[ "__unm" \] = 1) \}) nil))) 
                   : (1 ("q")))))
  
  ; E-NegationWrongOperandsNoHandler
  (test-->> metatables-mechanism
            (term (() : ((- "q")NegWrongOp)))
            (term (() : ($builtIn error
                                   ("attempt to perform arithmetic on a string value.")))))
  
  ; E-StringConcatWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil))) 
                   : ((1 .. "q")StrConcatWrongOps)))
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil))) 
                   : (1 (1 "q")))))
  
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil))) 
                   : (("q" .. 1)StrConcatWrongOps)))
            (term ((((objr 1) ((\{ (\[ "__concat" \] = 1) \}) nil))) 
                   : (1 ("q" 1)))))
  
  ; E-StringConcatWrongOperandsWithHandler
  (test-->> metatables-mechanism
            (term (() : ((1 .. "q")StrConcatWrongOps)))
            (term (() : ($builtIn error ("attempt to concatenate a number value.")))))
  
  ; E-StringLengthWrongOperandWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ (\[ "__len" \] = 1) \}) nil))) 
                   : ((\# 1)StrLenWrongOp)))
            (term ((((objr 1) ((\{ (\[ "__len" \] = 1) \}) nil))) 
                   : (1 (1)))))
  
  ; E-StringLengthWrongOperandTableLength
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ((\# (objr 1))StrLenWrongOp)))
            (term ((((objr 1) ((\{ \}) nil))) 
                   : 0)))
  
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ "a" \] = 1)
                                   (\[ 2 \] = 1) \}) nil))) 
                   : ((\# (objr 1))StrLenWrongOp)))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ "a" \] = 1)
                                   (\[ 2 \] = 1) \}) nil))) 
                   : 2)))
  
  ; E-StringLengthWrongOperandNoHandler
  (test-->> metatables-mechanism
            (term (() 
                   : ((\# 1)StrLenWrongOp)))
            (term (() 
                   : ($builtIn error ("attempt to get length of a number value.")))))
  
  ; E-EqualityFailWithHandler
  (test-->> metatables-mechanism
            (term (() 
                   : ((1 == 2)EqFail)))
            (term (() 
                   : false)))
  
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) nil)) 
                    ((objr 2) ((\{ \}) nil))) 
                   : (((objr 1) == (objr 2))EqFail)))
            
            (term ((((objr 1) ((\{ \}) nil)) 
                    ((objr 2) ((\{ \}) nil))) 
                   : false)))
  
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) (objr 3))) 
                    ((objr 2) ((\{ \}) (objr 3)))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil))) 
                   : (((objr 1) == (objr 2))EqFail)))
            
            (term ((((objr 1) ((\{ \}) (objr 3))) 
                    ((objr 2) ((\{ \}) (objr 3)))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil))) 
                   : (1 ((objr 1) (objr 2))))))
  
  ; E-EqualityFailNoHandler
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) (objr 3))) 
                    ((objr 2) ((\{ \}) nil))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil))) 
                   : (((objr 1) == (objr 2))EqFail)))
            
            (term ((((objr 1) ((\{ \}) (objr 3))) 
                    ((objr 2) ((\{ \}) nil))
                    ((objr 3) ((\{ (\[ "__eq" \] = 1) \}) nil))) 
                   : false)))
  
  ; E-LessThanFailWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__lt" \] = 1) \}) nil))) 
                   : (("a" < 1)OrdCompWrongOps)))
            (term ((((objr 4) ((\{ (\[ "__lt" \] = 1) \}) nil))) 
                   : (1 ("a" 1)))))
  
  ; E-LessThanFailNoHandler
  (test-->> metatables-mechanism
            (term (() : (("a" < 1)OrdCompWrongOps)))
            (term (() : ($builtIn error ("attempt to compare string with number")))))
  
  ; E-LessThanOrEqualFailWithHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__le" \] = 1) \}) nil))) 
                   : (("a" <= 1)OrdCompWrongOps)))
            
            (term ((((objr 4) ((\{ (\[ "__le" \] = 1) \}) nil))) 
                   : (1 ("a" 1)))))
  
  ; E-LessThanOrEqualFailWithAltHandler
  (test-->> metatables-mechanism
            (term ((((objr 4) ((\{ (\[ "__le" \] = nil)
                                   (\[ "__lt" \] = 1) \}) nil))) 
                   : (("a" <= 1)OrdCompWrongOps)))
            
            (term ((((objr 4) ((\{ (\[ "__le" \] = nil)
                                   (\[ "__lt" \] = 1) \}) nil))) 
                   : (not (1 (1 "a"))))))
  
  ; E-LessThanOrEqualFailNoHandler
  (test-->> metatables-mechanism
            (term (() : (("a" <= 1)OrdCompWrongOps)))
            (term (() : ($builtIn error ("attempt to compare string with number")))))

  ; E-FieldAssignWrongKeyNormal
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 2) ((\{ \}) (objr 1)))
                    ((objr 6) (function X () ((ref 1) ()) end))) 
                   : (((((objr 2) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 2) ((\{ \}) (objr 1)))
                    ((objr 6) (function X () ((ref 1) ()) end))) 
                   : ((objr 6) ((objr 2) 1 2)))))
  
  ; E-FieldAssignWrongKeyRepeat
  (test-->> metatables-mechanism
            (term ((((objr 1)
                     ((\{ (\[ "__newindex" \] = (objr 3)) \}) nil))
                    ((objr 2)
                     ((\{ \}) (objr 1)))
                    ((objr 3)
                     ((\{ (\[ 1 \] = 1) \}) nil)))
                   : (((((objr 2) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1)
                     ((\{ (\[ "__newindex" \] = (objr 3)) \}) nil))
                    ((objr 2)
                     ((\{ \}) (objr 1)))
                    ((objr 3)
                     ((\{ (\[ 1 \] = 1) \}) nil))) 
                   : ((((objr 3) \[ 1 \])) = (2)))))
  
  ; E-FieldAssignWrongKeyNoHandler
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) nil))
                    ((objr 2) ((\{ \}) (objr 1)))) 
                   : (((((objr 2) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ \}) nil))
                    ((objr 2) ((\{ (\[ 1 \] = 2) \}) (objr 1)))) 
                   : (< (objr 2) >))))
  
  (test-->> metatables-mechanism
            (term ((((objr 1) ((\{ \}) nil)))
                   : (((((objr 1) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : (< (objr 1) >))))
  
  ; E-FieldAssignOverNonTableNormal
  (test-->> metatables-mechanism
            (term ((((objr 1) 
                     ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 6) 
                     (function X () ((ref 1) ()) end)))
                   : ((((1 \[ 2 \])) = (2))FieldAssignOverNonTable)))
            
            (term ((((objr 1) 
                     ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 6) 
                     (function X () ((ref 1) ()) end)))
                   : ((objr 6) (1 2 2)))))
  
    ; E-FieldAssignOverNonTableRepeat
    (test-->> metatables-mechanism
              (term ((((objr 1)
                       ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                      ((objr 6)
                       ((\{ \}) nil)))
                     : ((((1 \[ 2 \])) = (3))FieldAssignOverNonTable)))
              
              (term ((((objr 1)
                       ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                      ((objr 6)
                       ((\{ \}) nil)))
                     : ((((objr 6) \[ 2 \])) = (3)))))
    
    ; E-FieldAssignOverNonTableNoHandler
    (test-->> metatables-mechanism
              (term (() : ((((1 \[ 2 \])) = (3))FieldAssignOverNonTable)))
              (term (() : ($builtIn error
                                    ("attempt to index a number value"))))

              )
    
    (test-->> metatables-mechanism
              (term ((((objr 1) ((\{ \}) nil)))
                     : ((((1 \[ 2 \])) = (3))FieldAssignOverNonTable)))


              (term ((((objr 1) ((\{ \}) nil)))
                     : ($builtIn error ("attempt to index a number value"))))
              )
  

  (test-results))

(provide metatables-mechanism-test-suite)
