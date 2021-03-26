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
  (test-equal (term (substExp (function A () ($statFCall X ()) end)
                              ((X (ref 1))))) 
              (term (function A () ($statFCall (ref 1) ()) end)))
  
  (test-equal (term (substExp (function A (X Y) (($statFCall X ())
                                                 ($statFCall Z ())) 
                                 end) ((X (ref 1))))) 
              (term (function A (X Y) (($statFCall X ())
                                       ($statFCall Z ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<)
                                        (($statFCall X ())
                                         ($statFCall Z ())) 
                                 end) ((X (ref 1)))))
              (term (function A (X Y <<<)
                              (($statFCall X ())
                               ($statFCall Z ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<)
                                        (($statFCall X ())
                                         ($statFCall Z ())) 
                                 end) ((Z (ref 1)) (<<< (ref 2)))))
              (term (function A (X Y <<<) (($statFCall X ())
                                           ($statFCall (ref 1) ())) end)))
  
  (test-equal (term (substExp (function A (X Y)
                                        (($statFCall X ())
                                         ($statFCall Z ())) 
                                 end) ((Z (ref 1))))) 
              (term (function A (X Y) (($statFCall X ())
                                       ($statFCall (ref 1) ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<)
                                        (($statFCall X ())
                                         ($statFCall Z ())) 
                                        end) ((Z (ref 1))))) 
              (term (function A (X Y <<<) (($statFCall X ())
                                           ($statFCall (ref 1) ())) end)))
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
  (test-equal (term (substBlock ($statFCall X ()) ((X (ref 1)))))
              (term ($statFCall (ref 1) ())))
  (test-equal (term (substBlock ($statFCall X (1 2 3)) ((X (ref 1)))))
              (term ($statFCall (ref 1) (1 2 3))))
  (test-equal (term (substBlock ($statFCall (\( <<< \)) ()) ((<<< (ref 1)))))
                    (term ($statFCall (\( (ref 1) \)) ())))
  
  ; concatenation statement
  (test-equal (term (substBlock (($statFCall X ())
                                 ($statFCall Y ()))
                                ((X (ref 1)) (Y (ref 2))))) 
              (term (($statFCall (ref 1) ())
                     ($statFCall (ref 2) ()))))
  ; block Bo-End
  (test-equal (term (substBlock (do ($statFCall X ()) end) ((X (ref 1))))) 
              (term (do ($statFCall (ref 1) ()) end)))
  ; return statement
  (test-equal (term (substBlock (return X) ((X (ref 1))))) 
              (term (return (ref 1))))
  
  (test-equal (term (substBlock (return <<<) ((<<< (< 1 >))))) 
              (term (return (< 1 >))))
  ; conditional
  (test-equal (term (substBlock (if X then ($statFCall Y ())
                                    else ($statFCall Z ()) end) 
                                ((X (ref 1)) (Y (ref 2)) (Z (ref 3)))))
              (term (if (ref 1) then ($statFCall (ref 2) ())
                        else ($statFCall (ref 3) ()) end)))
  
  (test-equal (term (substBlock (if <<< then ($statFCall Y ())
                                    else ($statFCall Z ()) end) 
                                ((Y (ref 1)) (Z (ref 2)) (<<< (ref 3)))))
              (term (if (ref 3) then ($statFCall (ref 1) ())
                        else ($statFCall (ref 2) ()) end)))
  ; while loop
  (test-equal (term (substBlock (while X do ($statFCall Y ()) end) 
                                ((X (ref 1)) (Y (ref 2)))))
              (term (while (ref 1) do ($statFCall (ref 2) ()) end)))
  
  (test-equal (term (substBlock (while <<< do ($statFCall Y ()) end)
                                ((Y (ref 1)) (<<< (ref 2)))))
              (term (while (ref 2) do ($statFCall (ref 1) ()) end)))
  ; local statement
  (test-equal (term (substBlock (local X Y = X Y in
                                  (($statFCall X ())
                                   ($statFCall Y ())) end) 
                                ((X (ref 1)) (Y (ref 2)))))
              (term (local X Y = (ref 1) (ref 2) in
                      (($statFCall X ())
                       ($statFCall Y ())) end)))
  
  (test-equal (term (substBlock (local X Y = X Y in
                                  (($statFCall U ())
                                   ($statFCall V ())  
                                   ($statFCall X ()) 
                                   ($statFCall Y ())) end) 
                                ((U (ref 1)) (V (ref 2)))))
              (term (local X Y = X Y in
                      (($statFCall (ref 1) ()) 
                      ($statFCall (ref 2) ()) 
                      ($statFCall X ())
                      ($statFCall Y ())) end)))
  
  (test-equal (term (substBlock (local X Y = X Y in
                                  (($statFCall U ())
                                   ($statFCall V (<<<)) 
                                   ($statFCall X ())
                                   ($statFCall Y ())) end) 
                                ((U (ref 1)) (V (ref 2)) (<<< (ref 3)))))
              (term (local X Y = X Y in
                      (($statFCall (ref 1) ())
                       ($statFCall (ref 2) ((ref 3)))
                       ($statFCall X ())
                       ($statFCall Y ())) end)))
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
