#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/termsObjStore.rkt")

(define (terms-obj-store-test-suite)
  ; Table creation
  (test-->> terms-obj-store
            (term (() : (\{ \})))
            (term ((((objr 7) ((\{ \}) nil ⊥))) : (objr 7))))
  
  (test-->> terms-obj-store
            (term (() : (\{ 1 (\[ 1 \] = 2) 2 (\[ 2 \] = 3) nil 4 \})))
            (term ((((objr 7) ((\{ (\[ 1 \] = 1) 
                                   (\[ 2 \] = 2) 
                                   (\[ 4 \] = 4) \}) nil ⊥))) 
                   : (objr 7))))

  (test-->> terms-obj-store
            (term (() : (\{ (\[ 1 \] = 2) (\[ 1 \] = 3) \})))
            (term ((((objr 7) ((\{ (\[ 1 \] = 3) \}) nil ⊥))) 
                   : (objr 7))))
  
  ; Table indexing
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 4 \] = 4) 
                                   \}) nil 0))) 
                   : ((objr 1) \[ 4 \])))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 4 \] = 4) 
                                   \}) nil 0))) : 4)))

  ; Coercion to string
  ;  (test-->> terms-obj-store
  ;            (term (() : ("1" .. 2.0)))
  ;            (term (() : ("1" .. "2"))))
  ;  
  ;  (test-->> terms-obj-store
  ;            (term (() : (1 .. "2.0")))
  ;            (term (() : ("1" .. "2.0"))))
  
  ; E-AlertWrongKey
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))) 
                   : ((objr 1) \[ 2 \])))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1) \}) (objr 2) 0))) 
                   : (((objr 1) \[ 2 \])WrongKey))))
  ; E-AlertNonTable
  (test-->> terms-obj-store
            (term (() : (1 \[ 2 \])))
            
            (term (() : ((1 \[ 2 \])NonTable))))

  ; Function creation
  (test-->> terms-obj-store
            (term (() : (function X () ($statFunCall Y ()) end)))
            (term ((((cl 7) (function X () ($statFunCall Y ()) end)))
                   : (cl 7))))

  (test-->> terms-obj-store
            (term ((((cl 1) (function X () ($statFunCall Y ()) end)))
                   : (function X () ($statFunCall Y ()) end)))

            (term ((((cl 1) (function X () ($statFunCall Y ()) end)))
                   : (cl 1))))

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

  ; getmetatable
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ($builtIn getmetatable (1))))
            
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : (objr 1))))
  
  (test-->> terms-obj-store
            (term (() : ($builtIn getmetatable (1))))
            
            (term (() : nil)))
  
  ; next
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ($builtIn next ((objr 1) nil))))
            
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : (< nil >))))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \})
                               nil 0))) 
                   : ($builtIn next ((objr 1) 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \})
                               nil 0))) 
                   : (< 3 4 >))))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) 
                                   (\[ 3 \] = 4) \})
                               nil 0))) 
                   : ($builtIn next ((objr 1) 2))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) 
                                   (\[ 3 \] = 4) \})
                               nil 0))) 
                   : ($err "invalid key to 'next'")))) 
  
  ; rawget
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($builtIn rawget ((objr 1) 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : 2)))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($builtIn rawget ((objr 1) 2))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : nil)))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($builtIn rawget (1 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($err  "bad argument #1 (table expected, got number)"))))
  
  ; rawset
  ; Alter field
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($builtIn rawset ((objr 1) 1 3))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 3) \}) nil 0))) 
                   : (objr 1))))
  ; Add new field
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($builtIn rawset ((objr 1) 2 3))))
            
            (term ((((objr 1) ((\{ (\[ 2 \] = 3) (\[ 1 \] = 2) \}) nil 0))) 
                   : (objr 1))))
  ; setmetatable
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))
                    ((objr 2) ((\{ \}) nil 0))) 
                   : ($builtIn setmetatable ((objr 1) (objr 2)))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) (objr 2) ⊥))
                    ((objr 2) ((\{ \}) nil 0))) 
                   : (objr 1))))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($builtIn setmetatable ((objr 1) 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil 0))) 
                   : ($err
                      "bad argument #2 to 'setmetatable' (nil or table expected)"))))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) (objr 2) 0))
                    ((objr 2) ((\{ (\[ "__metatable" \] = false) \}) nil 0))) 
                   : ($builtIn setmetatable ((objr 1) nil))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) (objr 2) 0))
                    ((objr 2) ((\{ (\[ "__metatable" \] = false) \}) nil 0))) 
                   : ($err "cannot change a protected metatable"))))

  ; tostring
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ \}) (objr 2) 0)) ((objr 2) ((\{ \}) nil 0)))
                   : ($builtIn tostring ((objr 1)))))
            
            (term ((((objr 1) ((\{ \}) (objr 2) 0)) ((objr 2) ((\{ \}) nil 0)))
                   : "table: (objr 1)")))

  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ \}) (objr 2) 0))
                    ((objr 2) ((\{ (\[ "__tostring" \] = "this ain't a function") \}) nil 0)))
                   : ($builtIn tostring ((objr 1)))))
            
            (term ((((objr 1) ((\{ \}) (objr 2) 0))
                    ((objr 2) ((\{ (\[ "__tostring" \] = "this ain't a function") \}) nil 0)))
                   : ("this ain't a function" ((objr 1))))))

  
  
 
  
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

  
  ; Table assignment
  ; Normal assignment
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil 0))) 
                   : (((objr 1) \[ 4 \]) = 9)))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 9) \}) nil 0))) 
                   : \;)))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil 0))) 
                   : (((objr 1) \[ 2 \]) = 10)))

            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 10)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil 0))) 
                   : \;)))
  
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil 0))) 
                   : (((objr 1) \[ 2 \]) = nil)))

            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil 0))) 
                   : \;)))
  
  ; Trying to index with key nil
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 3 \] = nil)
                                   (\[ 4 \] = 4) \}) nil 0))) 
                   : (((objr 1) \[ nil \]) = 1)))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 3 \] = nil)
                                   (\[ 4 \] = 4) \}) nil 0)))
                   : ((((objr 1) \[ nil \]) = 1)WrongKey))))
  
  ; E-AlertFieldAssignWrongKey
  (test-->> terms-obj-store
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : (((objr 1) \[ 1 \]) = 2)))
            
            (term ((((objr 1) ((\{ \}) nil 0))) 
                   : ((((objr 1) \[ 1 \]) = 2)WrongKey))))
  
  ; E-AlertFieldAssignOverNonTable
  (test-->> terms-obj-store
            (term (() : ((1 \[ 2 \]) = 3)))
            (term (() : (((1 \[ 2 \]) = 3)NonTable))))                                                                
  
  (test-results))

(provide terms-obj-store-test-suite)
