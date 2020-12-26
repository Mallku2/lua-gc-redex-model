#lang racket
(require redex
         "../Meta-functions/substitution.rkt")

; "black-box testing" with "equivalence class partitioning"

(define (subst-exp-test-suite)
  ; expression without structure
  (test-equal (term (substExp nil ((X (ref 1))))) (term nil))
  (test-equal (term (substExp true ((X (ref 1))))) (term true))
  (test-equal (term (substExp void ((X (ref 1))))) (term void))
  (test-equal (term (substExp "" ((X (ref 1))))) (term ""))
  (test-equal (term (substExp 1 ((X (ref 1))))) (term 1))
  (test-equal (term (substExp (objr 1) ((X (ref 1))))) (term (objr 1)))
  (test-equal (term (substExp (ref 1) ((X (ref 1))))) (term (ref 1)))
  (test-equal (term (substExp X ((X (ref 1))))) (term (ref 1)))
  (test-equal (term (substExp X ((Y (ref 1))))) (term X))
  (test-equal (term (substExp <<< ((<<< (ref 1))))) (term (ref 1)))
  ; function call
  (test-equal (term (substExp (X ()) ((X (ref 1))))) (term ((ref 1) ())))
  (test-equal (term (substExp (X (1 2 3)) ((X (ref 1))))) (term ((ref 1) (1 2 3))))
  (test-equal (term (substExp ((\( <<< \)) ()) ((<<< (ref 1)))))
                    (term ((\( (ref 1) \)) ())))
  ; '(' ')' operator
  (test-equal (term (substExp (\( X \)) ((X (ref 1))))) (term (\( (ref 1) \))))
  (test-equal (term (substExp (\( <<< \)) ((<<< (ref 1))))) (term (\( (ref 1) \))))
  ; table indexing
  (test-equal (term (substExp (X \[ Y \]) ((X (ref 1)) (Y (ref 2))))) 
              (term ((ref 1) \[ (ref 2) \])))
  (test-equal (term (substExp (X \[ <<< \]) ((X (ref 1)) (<<< (ref 2))))) 
              (term ((ref 1) \[ (ref 2) \])))
  ; tuples
  (test-equal (term (substExp (< X Y >) ((X (ref 1)) (Y (ref 2))))) 
              (term (< (ref 1) (ref 2) >)))
  (test-equal (term (substExp (< X <<< >) ((X (ref 1)) (<<< (ref 2))))) 
              (term (< (ref 1) (ref 2) >)))
  ; function definition
  (test-equal (term (substExp (function A () ($statFunCall X ()) end)
                              ((X (ref 1))))) 
              (term (function A () ($statFunCall (ref 1) ()) end)))
  
  (test-equal (term (substExp (function A (X Y) (($statFunCall X ())
                                                 ($statFunCall Z ())) 
                                 end) ((X (ref 1))))) 
              (term (function A (X Y) (($statFunCall X ())
                                       ($statFunCall Z ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<)
                                        (($statFunCall X ())
                                         ($statFunCall Z ())) 
                                 end) ((X (ref 1)))))
              (term (function A (X Y <<<)
                              (($statFunCall X ())
                               ($statFunCall Z ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<)
                                        (($statFunCall X ())
                                         ($statFunCall Z ())) 
                                 end) ((Z (ref 1)) (<<< (ref 2)))))
              (term (function A (X Y <<<) (($statFunCall X ())
                                           ($statFunCall (ref 1) ())) end)))
  
  (test-equal (term (substExp (function A (X Y)
                                        (($statFunCall X ())
                                         ($statFunCall Z ())) 
                                 end) ((Z (ref 1))))) 
              (term (function A (X Y) (($statFunCall X ())
                                       ($statFunCall (ref 1) ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<)
                                        (($statFunCall X ())
                                         ($statFunCall Z ())) 
                                        end) ((Z (ref 1))))) 
              (term (function A (X Y <<<) (($statFunCall X ())
                                           ($statFunCall (ref 1) ())) end)))
  ; table constructor
  (test-equal (term (substExp (\{ (\[ X \] = Y) (\[ Z \] = 1) \}) 
                              ((X (ref 1)) (Y (ref 2)) (Z (ref 3)))))
              (term (\{ (\[ (ref 1) \] = (ref 2)) (\[ (ref 3) \] = 1) \})))
  
  (test-equal (term (substExp (\{ (\[ X \] = Y) (\[ Z \] = 1) \}) 
                              ((X (ref 1)) (Y (ref 2)) (Z (ref 3)))))
              (term (\{ (\[ (ref 1) \] = (ref 2)) (\[ (ref 3) \] = 1) \})))
  
  (test-equal (term (substExp (\{ (\[ X \] = Y) (\[ <<< \] = 1) \}) 
                              ((X (ref 1)) (Y (ref 2)) (<<< (ref 3)))))
              (term (\{ (\[ (ref 1) \] = (ref 2)) (\[ (ref 3) \] = 1) \})))
  ; binary operators
  (test-equal (term (substExp (X + Y) ((X (ref 1)) (Y (ref 2))))) 
              (term ((ref 1) + (ref 2))))
  (test-equal (term (substExp (X + <<<) ((X (ref 1)) (<<< (ref 2))))) 
              (term ((ref 1) + (ref 2))))
  ; unary operators
  (test-equal (term (substExp (- X) ((X (ref 1))))) (term (- (ref 1))))
  (test-results))


(define (subst-block-test-suite)
  ; function call
  (test-equal (term (substBlock ($statFunCall X ()) ((X (ref 1)))))
              (term ($statFunCall (ref 1) ())))
  (test-equal (term (substBlock ($statFunCall X (1 2 3)) ((X (ref 1)))))
              (term ($statFunCall (ref 1) (1 2 3))))
  (test-equal (term (substBlock ($statFunCall (\( <<< \)) ()) ((<<< (ref 1)))))
                    (term ($statFunCall (\( (ref 1) \)) ())))
  
  ; concatenation statement
  (test-equal (term (substBlock (($statFunCall X ())
                                 ($statFunCall Y ()))
                                ((X (ref 1)) (Y (ref 2))))) 
              (term (($statFunCall (ref 1) ())
                     ($statFunCall (ref 2) ()))))
  ; block Bo-End
  (test-equal (term (substBlock (do ($statFunCall X ()) end) ((X (ref 1))))) 
              (term (do ($statFunCall (ref 1) ()) end)))
  ; return statement
  (test-equal (term (substBlock (return X) ((X (ref 1))))) 
              (term (return (ref 1))))
  
  (test-equal (term (substBlock (return <<<) ((<<< (< 1 >))))) 
              (term (return (< 1 >))))
  ; conditional
  (test-equal (term (substBlock (if X then ($statFunCall Y ())
                                    else ($statFunCall Z ()) end) 
                                ((X (ref 1)) (Y (ref 2)) (Z (ref 3)))))
              (term (if (ref 1) then ($statFunCall (ref 2) ())
                        else ($statFunCall (ref 3) ()) end)))
  
  (test-equal (term (substBlock (if <<< then ($statFunCall Y ())
                                    else ($statFunCall Z ()) end) 
                                ((Y (ref 1)) (Z (ref 2)) (<<< (ref 3)))))
              (term (if (ref 3) then ($statFunCall (ref 1) ())
                        else ($statFunCall (ref 2) ()) end)))
  ; while loop
  (test-equal (term (substBlock (while X do ($statFunCall Y ()) end) 
                                ((X (ref 1)) (Y (ref 2)))))
              (term (while (ref 1) do ($statFunCall (ref 2) ()) end)))
  
  (test-equal (term (substBlock (while <<< do ($statFunCall Y ()) end)
                                ((Y (ref 1)) (<<< (ref 2)))))
              (term (while (ref 2) do ($statFunCall (ref 1) ()) end)))
  ; local statement
  (test-equal (term (substBlock (local X Y = X Y in
                                  (($statFunCall X ())
                                   ($statFunCall Y ())) end) 
                                ((X (ref 1)) (Y (ref 2)))))
              (term (local X Y = (ref 1) (ref 2) in
                      (($statFunCall X ())
                       ($statFunCall Y ())) end)))
  
  (test-equal (term (substBlock (local X Y = X Y in
                                  (($statFunCall U ())
                                   ($statFunCall V ())  
                                   ($statFunCall X ()) 
                                   ($statFunCall Y ())) end) 
                                ((U (ref 1)) (V (ref 2)))))
              (term (local X Y = X Y in
                      (($statFunCall (ref 1) ()) 
                      ($statFunCall (ref 2) ()) 
                      ($statFunCall X ())
                      ($statFunCall Y ())) end)))
  
  (test-equal (term (substBlock (local X Y = X Y in
                                  (($statFunCall U ())
                                   ($statFunCall V (<<<)) 
                                   ($statFunCall X ())
                                   ($statFunCall Y ())) end) 
                                ((U (ref 1)) (V (ref 2)) (<<< (ref 3)))))
              (term (local X Y = X Y in
                      (($statFunCall (ref 1) ())
                       ($statFunCall (ref 2) ((ref 3)))
                       ($statFunCall X ())
                       ($statFunCall Y ())) end)))
  ; variable assignment
  (test-equal (term (substBlock (X Y = U V)
                                ((X (ref 1)) (Y (ref 2)) (U (ref 3)) (V (ref 4)))))
              (term ((ref 1) (ref 2) = (ref 3) (ref 4))))
  
  (test-equal (term (substBlock ((X \[ Y \]) = U V)
                                ((X (ref 1)) (Y (ref 2)) (U (ref 3)) (V (ref 4)))))
              (term (((ref 1) \[ (ref 2) \]) = (ref 3) (ref 4))))
  
  (test-equal (term (substBlock (X Y = U <<<)
                                ((X (ref 1)) (Y (ref 2)) (U (ref 3)) (<<< (ref 4)))))
              (term ((ref 1) (ref 2) = (ref 3) (ref 4))))
  
  (test-results))

(define (subs-test-suite)
  (subst-block-test-suite)
  (subst-exp-test-suite)
  )

(provide subs-test-suite)
