#lang racket
; Black-box testing for simple statements

(require redex
         "../grammar.rkt"
         "../Relations/terms.rkt")

(define (terms-rel-test-suite)
  
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

  ; Tuples
  ; Operator ()
  (test-->> terms-rel
            (term (\( (< 1 2 3 >) \)))
            1)
  (test-->> terms-rel
            (term (\( (< >) \)))
            (term nil))
  (test-->> terms-rel
            (term (\( 1 \)))
            (term 1))

  ; other ops. over tuples
  ; truncation
  (test-->> terms-rel
            (term ((1 \[ (< 1 2 >) \]) = 3))
            (term ((1 \[ 1 \]) = 3)))

  (test-->> terms-rel
            (term (((< 1 2 >) \[ 3 \]) = 4))
            (term ((1 \[ 3 \]) = 4)))
  
  (test-->> terms-rel
            (term (local x y = (< 1 2 >) 3 in \; end))
            (term (local x y = 1 3 in \; end)))

  (test-->> terms-rel
            (term (local x y = (< >) 1 in \; end))
            (term (local x y = nil 1 in \; end)))

  (test-->> terms-rel
            (term (local x y = (< >) in \; end))
            (term (local x y = nil nil in \; end)))

  (test-->> terms-rel
            (term (\{ 1 (< 2 3 >) 4 \}))
            (term (\{ 1 2 4 \})))

  (test-->> terms-rel
            (term (\{ 1 (\[ 2 \] = (< 3 4 >)) \}))
            (term (\{ 1 (\[ 2 \] = 3) \})))

  (test-->> terms-rel
            (term (\{ 1 (\[ (< 2 3 >) \] = 4) \}))
            (term (\{ 1 (\[ 2 \] = 4) \})))

  ; unwrap
  (test-->> terms-rel
            (term (local x y = (< 1 2 >) in \; end))
            (term (local x y = 1 2 in \; end)))
  
  (test-->> terms-rel
            (term (\{ 1 (< 2 3 >)\}))
            (term (\{ 1 2 3 \})))
  
  ; Arithmetic Operations
  (test-->> terms-rel
            (term (1 + 1))
            (term 2.0))
  (test-->> terms-rel
            (term (1 - 1))
            (term 0.0))
  (test-->> terms-rel
            (term (1 * 1))
            (term 1.0))
  (test-->> terms-rel
            (term (1 / 1))
            (term 1.0))
  (test-->> terms-rel
            (term (1 / 0))
            (term +inf.0))
  (test-->> terms-rel
            (term (1 ^ 1))
            (term 1.0))
  (test-->> terms-rel
            (term (1 % 1))
            (term 0.0))
  (test-->> terms-rel
            (term (1 % 0))
            (term +nan.0))
  ; Equality comparison
  (test-->> terms-rel
            (term (1 == 1))
            (term true))
  (test-->> terms-rel
            (term ("a" == "a"))
            (term true))
  ; Number order comparison
  (test-->> terms-rel
            (term (1 < 2))
            (term true))
  (test-->> terms-rel
            (term (2 > 1))
            (term true))
  (test-->> terms-rel
            (term (2 < 1))
            (term false))
  (test-->> terms-rel
            (term (2 <= 1))
            (term false))
  (test-->> terms-rel
            (term (1 >= 2))
            (term false))
  (test-->> terms-rel
            (term (2 >= 2))
            (term true))
  (test-->> terms-rel
            (term (1 <= 2))
            (term true))
  ; String order comparison
  (test-->> terms-rel
            (term ("a" < "a"))
            (term false))
  (test-->> terms-rel
            (term ("a" > "a"))
            (term false))
  (test-->> terms-rel
            (term ("a" < "b"))
            (term true))
  (test-->> terms-rel
            (term ("a" <= "a"))
            (term true))
  (test-->> terms-rel
            (term ("a" <= "b"))
            (term true))
  (test-->> terms-rel
            (term ("b" >= "a"))
            (term true))
  ; String concatenation
  (test-->> terms-rel
            (term ("a" .. "b"))
            (term "ab"))
  (test-->> terms-rel
            (term ("" .. "b"))
            (term "b"))
  ; coercion
  (test-->> terms-rel
            (term ("1" .. 2.0))
            (term "12"))
  
  (test-->> terms-rel
            (term (1 .. "2.0"))
            (term "12.0"))
  
  (test-->> terms-rel
            (term (1 .. 2))
            (term "12"))

  (test-->> terms-rel
            (term ($builtIn string.len (1234)))
            (term 4))
  ; String length
  (test-->> terms-rel
            (term (\# "a"))
            (term 1))
  ; Logical conectives
  (test-->> terms-rel
            (term (1 and (X ())))
            (term (\( (X ()) \))))
  (test-->> terms-rel
            (term (nil and 2))
            (term nil))
  (test-->> terms-rel
            (term (true and (ref 2)))
            (term (\( (ref 2) \))))
  (test-->> terms-rel
            (term (false and 2))
            (term false))
  (test-->> terms-rel
            (term (1 or 2))
            (term 1))
  (test-->> terms-rel
            (term (false or 2))
            (term 2))
  (test-->> terms-rel
            (term (nil or 2))
            (term 2))
  (test-->> terms-rel
            (term (not 1))
            (term false))
  (test-->> terms-rel
            (term (not nil))
            (term true))
  (test-->> terms-rel
            (term (not false))
            (term true))
  ; Coercion
  (test-->> terms-rel
            (term ("0x2.0p0" + 1.0))
            (term 3.0))
  (test-->> terms-rel
            (term ("   0x2.0p0   " + 1.0))
            (term 3.0))
  (test-->> terms-rel
            (term (1 + "0x1.0p0"))
            (term 2.0))
  (test-->> terms-rel
            (term ("0x1.0p0" - 1))
            (term 0.0))
  (test-->> terms-rel
            (term (1 - "0x1.0p0"))
            (term 0.0))
  (test-->> terms-rel
            (term ("0x1.0p0" * 1))
            (term 1.0))
  (test-->> terms-rel
            (term (1 * "0x1.0p0"))
            (term 1.0))
  (test-->> terms-rel
            (term ("0x1.0p0" / 1))
            (term 1.0))
  (test-->> terms-rel
            (term (1.0 / "0x1.0p0"))
            (term 1.0))
  (test-->> terms-rel
            (term ("0x1.0p0" ^ 1.0))
            (term 1.0))
  (test-->> terms-rel
            (term (1.0 ^ "0x1.0p0"))
            (term 1.0))
  (test-->> terms-rel
            (term ("0x1.0p0" % 1.0))
            (term 0.0))
  (test-->> terms-rel
            (term (1.0 % "0x1.0p0"))
            (term 0.0))
  (test-->> terms-rel
            (term (- "0x1.0p0"))
            (term -1.0))
  
  
  ; Abnormal expressions
  (test-->> terms-rel
            (term ("a" + 1))
            (term (("a" + 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 + "a"))
            (term ((1 + "a")ArithWrongOps)))
  (test-->> terms-rel
            (term ("0xq" + 1))
            (term (("0xq" + 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 + "0xq"))
            (term ((1 + "0xq")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 + "0x1.q"))
            (term ((1 + "0x1.q")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 + "0x1.1pq"))
            (term ((1 + "0x1.1pq")ArithWrongOps)))
  (test-->> terms-rel
            (term ("a" - 1))
            (term (("a" - 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 - "a"))
            (term ((1 - "a")ArithWrongOps)))
  (test-->> terms-rel
            (term ("0xq" - 1))
            (term (("0xq" - 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 - "0xq"))
            (term ((1 - "0xq")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 - "0x1.q"))
            (term ((1 - "0x1.q")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 - "0x1.1pq"))
            (term ((1 - "0x1.1pq")ArithWrongOps)))
  (test-->> terms-rel
            (term ("a" * 1))
            (term (("a" * 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 * "a"))
            (term ((1 * "a")ArithWrongOps)))
  (test-->> terms-rel
            (term ("0xq" * 1))
            (term (("0xq" * 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 * "0xq"))
            (term ((1 * "0xq")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 * "0x1.q"))
            (term ((1 * "0x1.q")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 * "0x1.1pq"))
            (term ((1 * "0x1.1pq")ArithWrongOps)))
  (test-->> terms-rel
            (term ("a" ^ 1))
            (term (("a" ^ 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 ^ "a"))
            (term ((1 ^ "a")ArithWrongOps)))
  (test-->> terms-rel
            (term ("0xq" ^ 1))
            (term (("0xq" ^ 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 ^ "0xq"))
            (term ((1 ^ "0xq")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 ^ "0x1.q"))
            (term ((1 ^ "0x1.q")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 ^ "0x1.1pq"))
            (term ((1 ^ "0x1.1pq")ArithWrongOps)))
  (test-->> terms-rel
            (term ("a" % 1))
            (term (("a" % 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 % "a"))
            (term ((1 % "a")ArithWrongOps)))
  (test-->> terms-rel
            (term ("0xq" % 1))
            (term (("0xq" % 1)ArithWrongOps)))
  (test-->> terms-rel
            (term (1 % "0xq"))
            (term ((1 % "0xq")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 % "0x1.q"))
            (term ((1 % "0x1.q")ArithWrongOps)))
  (test-->> terms-rel
            (term (1 % "0x1.1pq"))
            (term ((1 % "0x1.1pq")ArithWrongOps)))
  (test-->> terms-rel
            (term (- "a"))
            (term ((- "a")NegWrongOp)))
  (test-->> terms-rel
            (term (- "0xq"))
            (term ((- "0xq")NegWrongOp)))
  (test-->> terms-rel
            (term (- "0x1.q"))
            (term ((- "0x1.q")NegWrongOp)))
  (test-->> terms-rel
            (term (- "0x1.1pq"))
            (term ((- "0x1.1pq")NegWrongOp)))
  (test-->> terms-rel
            (term ("a" .. (objr 1)))
            (term (("a" .. (objr 1))StrConcatWrongOps)))
  (test-->> terms-rel
            (term (\# (objr 1)))
            (term ((\# (objr 1))StrLenWrongOp)))
  (test-->> terms-rel
            (term ("a" == "b"))
            (term false))
  (test-->> terms-rel
            (term (true == 1))
            (term false))
  (test-->> terms-rel
            (term ((objr 1) < (objr 1)))
            (term (((objr 1) < (objr 1))OrdCompWrongOps)))
  (test-->> terms-rel
            (term ((objr 1) <= (objr 1)))
            (term (((objr 1) <= (objr 1))OrdCompWrongOps)))

  ;                                                                                                                          
  ;   ;                         ;                        ;;                                     ;                            
  ;   ;                                                 ;                                                                    
  ;   ;                                                 ;                               ;                                    
  ;   ;                                                 ;                               ;                                    
  ;   ;;;;;     ;;;    ;;;;   ;;;       ;;;           ;;;;;   ;    ;  ; ;;;     ;;;   ;;;;;;  ;;;      ;;;;   ; ;;;    ;;;;  
  ;   ;;  ;;   ;   ;  ;    ;    ;      ;   ;            ;     ;    ;  ;;   ;   ;   ;    ;       ;     ;;  ;;  ;;   ;  ;    ; 
  ;   ;    ;       ;  ;         ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;  ;      
  ;   ;    ;   ;;;;;   ;;;;     ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;   ;;;;  
  ;   ;    ;  ;    ;       ;    ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;       ; 
  ;   ;;  ;;  ;   ;;  ;    ;    ;      ;   ;            ;     ;   ;;  ;    ;   ;   ;    ;       ;     ;;  ;;  ;    ;  ;    ; 
  ;   ;;;;;    ;;; ;   ;;;;   ;;;;;     ;;;             ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;    ;;;;   ;    ;   ;;;;  
  ;                                                                                                                          
  ;                                                                                                                          
  ;                                                                                                                          

  ; error
  (test-->> terms-rel
            (term ($builtIn error ("error message")))
            (term ($err "error message")))
  
  ; assert
  (test-->> terms-rel
            (term ($builtIn assert (false nil)))
            
            (term ($err "assertion failed!")))
  
  (test-->> terms-rel
            (term ($builtIn assert (false "this assertion is false")))
            
            (term ($err "this assertion is false")))
  
  (test-->> terms-rel
            (term ($builtIn assert (nil nil)))
            
            (term ($err "assertion failed!")))
  
  (test-->> terms-rel
            (term ($builtIn assert (true "this assertion is true")))
            
            (term (< true "this assertion is true" >)))
  
  (test-->> terms-rel
            (term ($builtIn assert (1 "this assertion is true")))
            
            (term (< 1 "this assertion is true" >)))
  
  ; table.pack
  (test-->> terms-rel
            (term ($builtIn table.pack ()))
            
            (term (\{ (\[ "n" \] = 0) \})))
  
  (test-->> terms-rel
            (term ($builtIn table.pack (1)))
            
            (term (\{ (\[ 1 \] = 1) (\[ "n" \] = 1) \})))
  
  (test-->> terms-rel
            (term ($builtIn table.pack (1 2)))
            
            (term (\{ (\[ 1 \] = 1) (\[ 2 \] = 2) (\[ "n" \] = 2) \})))
  
  ; pcall
  (test-->> terms-rel
            (term ($builtIn pcall ((objr 1) 1 2)))
            
            (term ($builtIn
                   xpcall
                   ((objr 1)
                    (function $handler (errMsg) (return false errMsg) end)
                    1
                    2))))
  
  ; tonumber
  (test-->> terms-rel
            (term ($builtIn tonumber (1 10)))
            
            (term 1))
  
  (test-->> terms-rel
            (term ($builtIn tonumber ("1" 10)))
            
            (term 1))
  
  (test-->> terms-rel
            (term ($builtIn tonumber ("0x1" 10)))
            
            (term nil))
  
  ; select
  (test-->> terms-rel
            (term ($builtIn select ("#" 1 2 3)))
            
            (term (< 3 >)))
  
  (test-->> terms-rel
            (term ($builtIn select (1 1 2 3)))
            
            (term (< 1 2 3 >)))
  
  (test-->> terms-rel
            (term ($builtIn select (3 1 2 3)))
            
            (term (< 3 >)))
  
  (test-->> terms-rel
            (term ($builtIn select (-1 1 2 3)))
            
            (term (< 3 >)))
  
  (test-->> terms-rel
            (term ($builtIn select (-3 1 2 3)))
            
            (term (< 1 2 3 >)))
  
  (test-->> terms-rel
            (term ($builtIn select (-5 1 2 3)))
            
            (term ($err "bad argument #1 to 'select' (index out of range)")))
  
  ; rawequal
  (test-->> terms-rel
            (term ($builtIn rawequal (true false)))
            
            (term false))
  
  (test-->> terms-rel
            (term ($builtIn rawequal ((objr 1) 1)))
            
            (term false))
  
  (test-->> terms-rel
            (term ($builtIn rawequal (1 1)))
            
            (term true))

  ; type
  (test-->> terms-rel
            (term ($builtIn type (1)))
            
            (term "number"))
  
  (test-->> terms-rel
            (term ($builtIn type ((objr 1))))
            
            (term "table"))
  
  (test-->> terms-rel
            (term ($builtIn type ((cl 1))))
            
            (term "function"))

  ;                                  
  ;                           ;      
  ;                           ;      
  ;                     ;     ;      
  ;                     ;     ;      
  ;   ;;;;;;;   ;;;   ;;;;;;  ; ;;;  
  ;   ;  ;  ;  ;   ;    ;     ;;   ; 
  ;   ;  ;  ;      ;    ;     ;    ; 
  ;   ;  ;  ;  ;;;;;    ;     ;    ; 
  ;   ;  ;  ; ;    ;    ;     ;    ; 
  ;   ;  ;  ; ;   ;;    ;     ;    ; 
  ;   ;  ;  ;  ;;; ;     ;;;  ;    ; 
  ;                                  
  ;                                  
  ;
  ; abs
  (test-->> terms-rel
            (term ($builtIn math.abs (-1)))
            
            (term 1))

  ; ceil
  (test-->> terms-rel
            (term ($builtIn math.ceil (2.3)))
            
            (term 3.0))

  ; cos
  (test-->> terms-rel
            (term ($builtIn math.cos (0)))
            
            (term 1))

  ; cosh
  (test-->> terms-rel
            (term ($builtIn math.cosh (0)))
            
            (term 1.0))

  ; deg
  (test-->> terms-rel
            (term ($builtIn math.deg (0)))
            
            (term 0))

  ; exp
  (test-->> terms-rel
            (term ($builtIn math.exp (0)))
            
            (term 1))

  ; fmod
  (test-->> terms-rel
            (term ($builtIn math.fmod (10 3)))
            
            (term 1))

  ; log
  (test-->> terms-rel
            (term ($builtIn math.log (1 nil)))
            
            (term 0.0))
  
  ; rad
  (test-->> terms-rel
            (term ($builtIn math.rad (0)))
            
            (term 0))

  ; sin
  (test-->> terms-rel
            (term ($builtIn math.sin (0)))
            
            (term 0))

  ; sinh
  (test-->> terms-rel
            (term ($builtIn math.sinh (0)))
            
            (term 0))

  ; sqrt
  (test-->> terms-rel
            (term ($builtIn math.sqrt (9)))
            
            (term 3))

  ; tan
  (test-->> terms-rel
            (term ($builtIn math.tan (0)))
            
            (term 0))

  ; tanh
  (test-->> terms-rel
            (term ($builtIn math.tanh (0)))
            
            (term 0))

  
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

  ; Conditional
  (test-->> terms-rel
            (term (if true then ($statFunCall X ())
                      else ($statFunCall Y ()) end))
            (term ($statFunCall X ())))
  (test-->> terms-rel
            (term (if (objr 1) then ($statFunCall X ())
                      else ($statFunCall Y ()) end))
            (term ($statFunCall X ())))
  (test-->> terms-rel
            (term (if false then ($statFunCall X ())
                      else ($statFunCall Y ()) end))
            (term ($statFunCall Y ())))
  (test-->> terms-rel
            (term (if nil then ($statFunCall X ())
                      else ($statFunCall Y ()) end))
            (term ($statFunCall Y ())))
  ; While loop
  (test-->> terms-rel
            (term (while false do
                         ($statFunCall X ()) end))
            (term (($iter false do
                                 ($statFunCall X ()) end) Break)))
  ; Concatenation of statements
  (test-->> terms-rel
            (term (\; ($statFunCall Y ())))
            (term ($statFunCall Y ())))
  ; Block Do-End
  (test-->> terms-rel
            (term (do \; end))
            (term \;))
  
  ; List length-equating rules
  ; E-AssignDiscardValues
  (test-->> terms-rel
            (term ((ref 1) = 1 2))
            (term ((ref 1) = 1)))
  ; E-AssignCompleteValues
  (test-->> terms-rel
            (term ((ref 1) (ref 2) = 1))
            (term (((ref 2) = nil) ((ref 1) = 1))))
  
  ; E-AssignSplit
  (test-->> terms-rel
            (term ((ref 1) (ref 2) = 1 2))
            (term (((ref 2) = 2) ((ref 1) = 1))))
  
  ; E-LocalDiscardValues
  (test-->> terms-rel
            (term (local X = 1 2 in \; end))
            (term (local X = 1 in \; end)))
  
  ; E-LocalCompleteValues
  (test-->> terms-rel
            (term (local X Y = 1 in \; end))
            (term (local X Y = 1 nil in \; end)))
  
  ; Break
  (test-->> terms-rel
            (term (\; Break))
            (term \;))
  
  (test-->> terms-rel
            (term (break Break))
            (term \;))

  ; Method call
  (test-->> terms-rel
            (term ((objr 1) : method_name (1)))
            
            (term (((objr 1) \[ "method_name" \]) ((objr 1) 1))))

  (test-->> terms-rel
            (term ((objr 1) : method_name (1)))
            
            (term (((objr 1) \[ "method_name" \]) ((objr 1) 1))))

  ; Return
  (test-->> terms-rel
            (term ((return 1 2) () RetStat))
            (term \;))

   (test-->> terms-rel
            (term (\; () RetStat))
            (term \;))

  (test-->> terms-rel
            (term ((return 1 2) () RetExp))
            (term (< 1 2 >)))

   (test-->> terms-rel
            (term (\; () RetExp))
            (term (< >)))
   
   (test-->> terms-rel
            (term ((return 1 2) Break))
            (term (return 1 2)))

  ; Protected mode
  (test-->> terms-rel
            (term ((< 1 >) ProtectedMode))
            (term (< true 1 >)))
  
  (test-->> terms-rel
            (term (($err 1) ProtectedMode))
            (term (< false 1 >)))

  (test-->> terms-rel
            (term (($err 1) ProtectedMode (cl 6)))
            (term ((cl 6) (1))))

  (test-->> terms-rel
            (term (($err 1) ProtectedMode 1))
            (term (< false "error in error handling" >)))


  (test-results))

(provide terms-rel-test-suite)
