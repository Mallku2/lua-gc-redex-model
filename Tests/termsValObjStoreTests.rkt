#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/termsValObjStore.rkt")

(define (terms-val-obj-store-test-suite)
  ; Function call
  ; Normal case
  (test-->> terms-val-obj-store
            (term ((((ref 1) 1) ((ref 2) 2)) 
                   :
                   (((cl 1) (function X (Y) ($statFunCall Y ()) end)))
                   :
                   ((cl 1) (2))))
            
            (term ((((ref 1) 1) ((ref 2) 2) ((ref 3) 2)) 
                   :
                   (((cl 1) (function X (Y) ($statFunCall Y ()) end)))
                   :
                   (($statFunCall (ref 3) ()) ((rEnv (ref 3))) RetExp))))
  
  ; More values than needed passed in the call
  (test-->> terms-val-obj-store
            (term ((((ref 1) 1) ((ref 2) 2)) 
                   :
                   (((cl 1) (function X (Y) ($statFunCall Y ()) end)))
                   :
                   ((cl 1) (2 1))))
            
            (term ((((ref 1) 1) ((ref 2) 2) ((ref 3) 2)) 
                   :
                   (((cl 1) (function X (Y) ($statFunCall Y ()) end)))
                   :
                   (($statFunCall (ref 3) ()) ((rEnv (ref 3))) RetExp))))
  
  ; Lesser values than needed passed in the call
  (test-->> terms-val-obj-store
            (term ((((ref 1) 1) ((ref 2) 2))  
                   :
                   (((cl 1) (function X (Y Z) ($statFunCall Z ()) end)))
                   :
                   ((cl 1) (2))))
            
            (term ((((ref 1) 1)
                    ((ref 2) 2)
                    ((ref 3) 2)
                    ((ref 4) nil)) 
                   :
                   (((cl 1) (function X (Y Z) ($statFunCall Z ()) end)))
                   :
                   (($statFunCall (ref 4) ())
                    ((rEnv (ref 3)) (rEnv (ref 4))) RetExp))))
  
  ; Vararg function: normal case
  (test-->> terms-val-obj-store
            (term (() 
                   :
                   (((cl 1) (function X (X <<<)
                                      ($statFunCall (\( <<< \)) ())
                                      end)))
                   :
                   ((cl 1) (1 2))))
            
            (term ((((ref 1) 1)) 
                   :
                   (((cl 1) (function X (X <<<)
                                      ($statFunCall (\( <<< \)) ())
                                      end)))
                   :
                   (($statFunCall (|(| (< 2 >) |)|) ())
                      ((rEnv (ref 1))) RetExp))))
  
  ; Vararg function: few arguments
  (test-->> terms-val-obj-store
            (term (()
                   :
                   (((cl 1) (function X (X Y <<<) ($statFunCall (\( <<< \)) ())
                                      end)))
                   :
                   ((cl 1) (1))))
            
            (term ((((ref 1) 1) ((ref 2) nil)) 
                   :
                   (((cl 1) (function X (X Y <<<) ($statFunCall (\( <<< \)) ())
                                      end)))
                   :
                   (($statFunCall (|(| (< >) |)|) ()) ((rEnv (ref 1))
                                                         (rEnv (ref 2)))
                                                        RetExp))))
  
  (test-results))

(provide terms-val-obj-store-test-suite)
