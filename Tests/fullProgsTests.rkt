#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullProgs.rkt")

(define (full-progs-rel-test-suite)
  ; full "while" loop
  (test-->> full-progs-rel
            (term ((((ref 1) 2))
                   : ()
                   : (while (1 < (ref 1)) do
                            ((ref 1) = ((ref 1) - 1))
                            end)))
            
            (term ((((ref 1) 1.0)) 
                   : () : \;)))
  
  (test-->> full-progs-rel
            (term ((((ref 1) 0)) 
                   : () : (while ((ref 1) < 2) do ((ref 1) = ((ref 1) + 1))
                                 end)))
            
            (term ((((ref 1) 2.0)) 
                   : () : \;)))
  
  ; Full while loop + short-circuit evaluation operator in the guard
  (test-->> full-progs-rel
            (term ((((ref 1) 0)
                    ((ref 2) true)) 
                   : () : (while (((ref 1) < 1) and (ref 2)) 
                                 do ((ref 1) = ((ref 1) + 1)) end)))
            
            (term ((((ref 1) 1.0)
                    ((ref 2) true)) 
                   : () : \;)))
  
  ; While loop + break
  (test-->> full-progs-rel
            (term ((((ref 1) 0)) 
                   : () : (while ((ref 1) < 2) 
                                 do (((ref 1) = ((ref 1) + 1))
                                     break) end)))
            
            (term ((((ref 1) 1.0)) 
                   : () : \;)))
  
  ; Function definition + function call
  (test-->> full-progs-rel
            (term ((((ref 1) nil)) 
                   : () 
                   : (((ref 1) = (function X () (return 1) end))
                      (if ((ref 1) ()) then 
                          ((ref 1) = 1) 
                          else ((ref 1) = 2) 
                          end))))
            ; TODO: first location is hard-coded...
            (term ((((ref 1) 1)) 
                   : (((cl 7) (function X () (return 1) end)))
                   : \;)))

  ; Vararg function definition + function call
  (test-->> full-progs-rel
            (term ((((ref 1) nil)) 
                   : () 
                   : (((ref 1) = (function X (<<<) (return <<<) end))
                      (if ((ref 1) (1 2 3)) then 
                          ((ref 1) = 1)
                          else ((ref 1) = 2) 
                          end))))
            
            ; TODO: first location is hard-coded...
            (term ((((ref 1) 1)) 
                   : (((cl 7) (function X (<<<) (return <<<) end)))
                   : \;)))

  ; From http://www.luafaq.org/: function with a modified _ENV as upvalue
  (test-->> full-progs-rel
            (term (() : () :  (local t = (\{ (\[ "x" \] = 10.000000 ) 
                                             (\[ "y" \] = 20.000000 ) \}) in 
                                (local f1 = nil in 
                                  (do 
                                      (local ENV = t in 
                                        ((f1 = (function $func1 ( )
                                                         (return
                                                          ((ENV \[ "x" \])
                                                           +
                                                           (ENV \[ "y" \])))
                                                         end))
                                         ((ENV \[ "z" \]) = (f1 ())))
                                        end) 
                                    end)
                                  end)
                                end)))

            (term ((((ref 1) (objr 7))
                    ((ref 2) (cl 8))
                    ((ref 3) (objr 7)))
                   :
                   (((objr 7) ((|{| (|[| "z" |]| = 30.0)
                                    (|[| "x" |]| = 10.0)
                                    (|[| "y" |]| = 20.0) |}|) nil ⊥))
                    ((cl 8)
                     (function $func1
                      ()
                      (return
                       (((ref 3) |[| "x" |]|) + ((ref 3) |[| "y" |]|)))
                      end)))
                   :
                   |;|)))
  ; Errors
  ; Propagation of an error message.
  (test-->> full-progs-rel
            (term ((((ref 1) nil)) 
                   : () 
                   : (((ref 1) = (function X (<<<)
                                           (return
                                            ($builtIn error ("error message")))
                                           end))
                      (if ((ref 1) (1 2 3)) then
                          ((ref 1) = 1)
                          else ((ref 1) = 2)
                          end))))

            (term ((((ref 1) (cl 7))) 
                   :
                   (((cl 7) (function X (<<<)
                                       (return
                                        ($builtIn error ("error message")))
                                       end))) 
                   : ($err "error message")))

            )
  
  (test-->> full-progs-rel
            (term (()
                   : ()
                   : (((return ($err "error"))
                       ((rEnv (ref 3))
                        (rEnv (ref 4))) RetStat) () RetStat)))
            (term (() : () : ($err "error"))))
  
  (test-->> full-progs-rel
            (term (() : () : (if ($builtIn error ("error"))
                                 then \;
                                 else \; end)))
            (term (() : () : ($err "error"))))
  
  (test-->> full-progs-rel
            (term (() : () : (local X = ($builtIn error ("error")) in \; end)))
            (term (() : () : ($err "error"))))

  ; protected Mode
  (test-->> full-progs-rel
            (term (()
                   : ()
                   : (return
                      ($builtIn pcall
                                ((function $1 ()
                                  (return ($builtIn error ("error")))
                                  end))))))
            (term ((((ref 1) "error"))
                   :
                   (((cl 7)
                     (function
                      $1
                      ()
                      (return ($builtIn error ("error")))
                      end))
                    ((cl 8)
                     (function
                      $handler
                      (errMsg)
                      (return false errMsg)
                      end)))
                   :
                   (return false "error"))))
  
  (test-->> full-progs-rel
            (term (()
                   : ()
                   : (return
                      ($builtIn xpcall
                                ((function x ()
                                           (return
                                            ($builtIn error ("error")))
                                           end)
                                 (function $1 (m)
                                           (local a = (m .. " received") in
                                             \;
                                             end)
                                           end))))))
            (term ((((ref 1) "error")
                    ((ref 2) "error received"))
                   :
                   (((cl 7)
                     (function x ()
                      (return ($builtIn error ("error")))
                      end))
                    ((cl 8)
                     (function $1 (m)
                               (local a = (m .. " received")
                                 in
                                 |;|
                                 end)
                               end)))
                   :
                   (return))))
 
;  (test-->> full-progs-rel
;            (term (() : () : (return ((< 1 >)
;                                      ProtectedMode
;                                      (function $1 (m)
;                                                (local a = (m .. " received")
;                                                  in |;| end) end)))))
;            (term (() : () : (return true 1))))
;  
;  (test-->> full-progs-rel
;            (term (()
;                   : ()
;                   : (return ($builtIn xpcall
;                                       ((function x () \; end)
;                                        (function $1 (m)
;                                                  (local
;                                                    a = (m .. " received")
;                                                    in \; end)
;                                                  end))))))
;            (term (()
;                   : ()
;                   : (return true))))

  (test-results))

(provide full-progs-rel-test-suite)
