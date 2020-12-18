#lang racket
(require redex
         "../grammar.rkt"
         "./objStoreMetafunctions.rkt"
         "./valStoreMetafunctions.rkt"
         "./grammarMetafunctions.rkt"
         "./coercion.rkt"
         "./gc.rkt"
         "./deltaBasic.rkt"
         "../Desugar/parser.rkt"
         "../Desugar/lexer.rkt"
         "../Desugar/phrases_constructors.rkt")


(define-metafunction ext-lang
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

  
  ;                          
  ;           ;              
  ;           ;              
  ;           ;              
  ;           ;              
  ;     ;;;   ;;;;;    ;;;;  
  ;    ;   ;  ;;  ;;  ;    ; 
  ;        ;  ;    ;  ;      
  ;    ;;;;;  ;    ;   ;;;;  
  ;   ;    ;  ;    ;       ; 
  ;   ;   ;;  ;;  ;;  ;    ; 
  ;    ;;; ;  ;;;;;    ;;;;  
  ;                          
  ;                          
  ;
  [(δmath math.abs String)
   (δmath math.abs Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.abs Number )
   ,(abs (term Number))]
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;     ;;;     ;;;    ;;;;    ;;;;  
  ;    ;   ;   ;   ;  ;;  ;;  ;    ; 
  ;        ;  ;       ;    ;  ;      
  ;    ;;;;;  ;       ;    ;   ;;;;  
  ;   ;    ;  ;       ;    ;       ; 
  ;   ;   ;;   ;   ;  ;;  ;;  ;    ; 
  ;    ;;; ;    ;;;    ;;;;    ;;;;  
  ;                                  
  ;                                  
  ;
  [(δmath math.acos String)
   (δmath math.acos Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.acos Number_1)
   Number_2

   ; check that the result is in the set of real numbers
   (where Number_2 ,(acos (term Number_1)))]

  ; parameter outside of [-1;1]
  [(δmath math.acos Number)
   +nan.0]
  
  ;                                  
  ;                     ;            
  ;                                  
  ;                                  
  ;                                  
  ;     ;;;    ;;;;   ;;;     ; ;;;  
  ;    ;   ;  ;    ;    ;     ;;   ; 
  ;        ;  ;         ;     ;    ; 
  ;    ;;;;;   ;;;;     ;     ;    ; 
  ;   ;    ;       ;    ;     ;    ; 
  ;   ;   ;;  ;    ;    ;     ;    ; 
  ;    ;;; ;   ;;;;   ;;;;;   ;    ; 
  ;                                  
  ;                                  
  ;
  [(δmath math.asin String)
   (δmath math.asin Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.asin Number_1)
   Number_2

   ; check that the result is in the set of real numbers (for parameter outside
   ; of [-1;1], the result will be a number with a non-zero complex part
   (where Number_2 ,(asin (term Number_1)))]

  ; parameter outside of [-1;1]
  [(δmath math.asin Number)
   +nan.0]
  
  ;                                  
  ;                                  
  ;                                  
  ;             ;                    
  ;             ;                    
  ;     ;;;   ;;;;;;    ;;;   ; ;;;  
  ;    ;   ;    ;      ;   ;  ;;   ; 
  ;        ;    ;          ;  ;    ; 
  ;    ;;;;;    ;      ;;;;;  ;    ; 
  ;   ;    ;    ;     ;    ;  ;    ; 
  ;   ;   ;;    ;     ;   ;;  ;    ; 
  ;    ;;; ;     ;;;   ;;; ;  ;    ; 
  ;                                  
  ;                                  
  ;
  [(δmath math.atan String)
   (δmath math.atan Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.atan Number)
   ,(atan (term Number))]

  
  ;                                  
  ;                     ;     ;;;    
  ;                             ;    
  ;                             ;    
  ;                             ;    
  ;     ;;;    ;;;;   ;;;       ;    
  ;    ;   ;  ;;  ;;    ;       ;    
  ;   ;       ;    ;    ;       ;    
  ;   ;       ;;;;;;    ;       ;    
  ;   ;       ;         ;       ;    
  ;    ;   ;  ;;   ;    ;       ;    
  ;     ;;;    ;;;;   ;;;;;      ;;; 
  ;                                  
  ;                                  
  ;
  [(δmath math.ceil String)
   (δmath math.ceil Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.ceil Number)
   ,(ceiling (term Number))]
  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;     ;;;    ;;;;    ;;;;  
  ;    ;   ;  ;;  ;;  ;    ; 
  ;   ;       ;    ;  ;      
  ;   ;       ;    ;   ;;;;  
  ;   ;       ;    ;       ; 
  ;    ;   ;  ;;  ;;  ;    ; 
  ;     ;;;    ;;;;    ;;;;  
  ;                          
  ;                          
  ;
  [(δmath math.cos String)
   (δmath math.cos Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.cos Number )
   ,(cos (term Number))]

  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;     ;;;    ;;;;    ;;;;   ; ;;;  
  ;    ;   ;  ;;  ;;  ;    ;  ;;   ; 
  ;   ;       ;    ;  ;       ;    ; 
  ;   ;       ;    ;   ;;;;   ;    ; 
  ;   ;       ;    ;       ;  ;    ; 
  ;    ;   ;  ;;  ;;  ;    ;  ;    ; 
  ;     ;;;    ;;;;    ;;;;   ;    ; 
  ;                                  
  ;                                  
  ;
  [(δmath math.cosh String)
   (δmath math.cosh Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.cosh Number )
   ,(cosh (term Number))]
  
  ;                          
  ;        ;                 
  ;        ;                 
  ;        ;                 
  ;        ;                 
  ;    ;;;;;   ;;;;    ;;;;; 
  ;   ;;  ;;  ;;  ;;  ;;  ;; 
  ;   ;    ;  ;    ;  ;    ; 
  ;   ;    ;  ;;;;;;  ;    ; 
  ;   ;    ;  ;       ;    ; 
  ;   ;;  ;;  ;;   ;  ;;  ;; 
  ;    ;;;;;   ;;;;    ;;; ; 
  ;                        ; 
  ;                    ;   ; 
  ;                     ;;;
  [(δmath math.deg String)
   (δmath math.deg Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.deg Number)
   ,(radians->degrees (term Number))]

  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;; 
  ;   ;    ;    ;;    ;    ; 
  ;   ;;;;;;    ;;    ;    ; 
  ;   ;         ;;    ;    ; 
  ;   ;;   ;   ;  ;   ;;  ;; 
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;                   ;      
  ;                   ;      
  ;                   ;
  [(δmath math.exp String)
   (δmath math.exp Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.exp Number)
   ,(exp (term Number))]

  ;                                          
  ;      ;;   ;;;                            
  ;     ;       ;                            
  ;     ;       ;                            
  ;     ;       ;                            
  ;   ;;;;;     ;      ;;;;    ;;;;    ;;;;  
  ;     ;       ;     ;;  ;;  ;;  ;;   ;;  ; 
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;;  ;;  ;;  ;;   ;     
  ;     ;        ;;;   ;;;;    ;;;;    ;     
  ;                                          
  ;                                          
  ;                                          
  [(δmath math.floor String)
   (δmath math.floor Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.floor Number )
   ,(floor (term Number))]
  
  ;                                  
  ;      ;;                        ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;   ;;;;;   ;;;;;;;  ;;;;    ;;;;; 
  ;     ;     ;  ;  ; ;;  ;;  ;;  ;; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;;  ;;  ;;  ;; 
  ;     ;     ;  ;  ;  ;;;;    ;;;;; 
  ;                                  
  ;                                  
  ;                                  
  [(δmath math.fmod Number_1 ... String v ...)
   (δmath math.fmod Number_1 ... Number_2 v ...)
   
   (where Number_2 (δbasic tonumber String nil))]
  
  [(δmath math.fmod Number_1 0)
   -nan.0]

  ; {Number_2 ≠ 0}
  [(δmath math.fmod Number_1 Number_2)
   ,(exact-floor (remainder (term Number_1) (term Number_2)))]
  
  ;                          
  ;   ;;;                    
  ;     ;                    
  ;     ;                    
  ;     ;                    
  ;     ;      ;;;;    ;;;;; 
  ;     ;     ;;  ;;  ;;  ;; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;;  ;;  ;;  ;; 
  ;      ;;;   ;;;;    ;;; ; 
  ;                        ; 
  ;                    ;   ; 
  ;                     ;;;
  [(δmath math.log Number_1 ... String v ...)
   (δmath math.log Number_1 ... Number_2 v ...)
   
   (where Number_2 (δbasic tonumber String nil))]

  [(δmath math.log Number_1 nil)
   ,(log (term Number_2))

   (where Number_2 ,(exact->inexact (term Number_1)))]

  [(δmath math.log Number_1 Number_2)
   (δbasic /
      (δmath math.log Number_3 nil)
      (δmath math.log Number_4 nil))

   (where Number_3 ,(exact->inexact (term Number_1)))
   (where Number_4 ,(exact->inexact (term Number_2)))]

  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;   ;;;;;;;   ;;;   ;;  ;; 
  ;   ;  ;  ;  ;   ;   ;  ;  
  ;   ;  ;  ;      ;    ;;   
  ;   ;  ;  ;  ;;;;;    ;;   
  ;   ;  ;  ; ;    ;    ;;   
  ;   ;  ;  ; ;   ;;   ;  ;  
  ;   ;  ;  ;  ;;; ;  ;;  ;; 
  ;                          
  ;                          
  ;
  [(δmath math.max Number_1 ... String v ...)
   (δmath math.max Number_1 ... Number_2 v ...)
   
   (where Number_2 (δbasic tonumber String nil))]
  
  [(δmath math.max Number ...)
   ,(foldr (λ (nmbr accum) (max nmbr accum))
           -inf.0
           (term (Number ...)))]
  ;                                  
  ;                        ;     ;;  
  ;                        ;    ;    
  ;                        ;    ;    
  ;                        ;    ;    
  ;   ;;;;;;;  ;;;;    ;;;;;  ;;;;;  
  ;   ;  ;  ; ;;  ;;  ;;  ;;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;;  ;;  ;;  ;;    ;    
  ;   ;  ;  ;  ;;;;    ;;;;;    ;    
  ;                                  
  ;                                  
  ;
  [(δmath math.modf String)
   (δmath math.modf Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.modf Number)
   ,(- (term Number) (exact-truncate (term Number)))]
  ;                          
  ;                        ; 
  ;                        ; 
  ;                        ; 
  ;                        ; 
  ;    ;;;;     ;;;    ;;;;; 
  ;    ;;  ;   ;   ;  ;;  ;; 
  ;    ;           ;  ;    ; 
  ;    ;       ;;;;;  ;    ; 
  ;    ;      ;    ;  ;    ; 
  ;    ;      ;   ;;  ;;  ;; 
  ;    ;       ;;; ;   ;;;;; 
  ;                          
  ;                          
  ;                          
  [(δmath math.rad String)
   (δmath math.rad Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.rad Number)
   ,(degrees->radians (term Number))]
  
  ;                          
  ;             ;            
  ;                          
  ;                          
  ;                          
  ;    ;;;;   ;;;     ; ;;;  
  ;   ;    ;    ;     ;;   ; 
  ;   ;         ;     ;    ; 
  ;    ;;;;     ;     ;    ; 
  ;        ;    ;     ;    ; 
  ;   ;    ;    ;     ;    ; 
  ;    ;;;;   ;;;;;   ;    ; 
  ;                          
  ;                          
  ;                          
  [(δmath math.sin String)
   (δmath math.sin Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.sin Number)
   ,(sin (term Number))]
  
  ;                                  
  ;             ;             ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;    ;;;;   ;;;     ; ;;;   ; ;;;  
  ;   ;    ;    ;     ;;   ;  ;;   ; 
  ;   ;         ;     ;    ;  ;    ; 
  ;    ;;;;     ;     ;    ;  ;    ; 
  ;        ;    ;     ;    ;  ;    ; 
  ;   ;    ;    ;     ;    ;  ;    ; 
  ;    ;;;;   ;;;;;   ;    ;  ;    ; 
  ;                                  
  ;                                  
  ;
  [(δmath math.sinh String)
   (δmath math.sinh Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.sinh Number)
   ,(sinh (term Number))]


  
  ;                                  
  ;                                  
  ;                                  
  ;                             ;    
  ;                             ;    
  ;    ;;;;    ;;;;;   ;;;;   ;;;;;; 
  ;   ;    ;  ;;  ;;   ;;  ;    ;    
  ;   ;       ;    ;   ;        ;    
  ;    ;;;;   ;    ;   ;        ;    
  ;        ;  ;    ;   ;        ;    
  ;   ;    ;  ;;  ;;   ;        ;    
  ;    ;;;;    ;;; ;   ;         ;;; 
  ;                ;                 
  ;                ;                 
  ;                ;
  [(δmath math.sqrt String)
   (δmath math.sqrt Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.sqrt Number)
   ,(sqrt (term Number))

   (side-condition (> (term Number) 0))]

  ; {Number >= 0}
  [(δmath math.sqrt Number)
   -nan.0]
  
  ;                          
  ;                          
  ;                          
  ;     ;                    
  ;     ;                    
  ;   ;;;;;;    ;;;   ; ;;;  
  ;     ;      ;   ;  ;;   ; 
  ;     ;          ;  ;    ; 
  ;     ;      ;;;;;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;   ;;  ;    ; 
  ;      ;;;   ;;; ;  ;    ; 
  ;                          
  ;                          
  ;
  [(δmath math.tan String)
   (δmath math.tan Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.tan Number)
   ,(tan (term Number))]
  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;     ;                     ;      
  ;     ;                     ;      
  ;   ;;;;;;    ;;;   ; ;;;   ; ;;;  
  ;     ;      ;   ;  ;;   ;  ;;   ; 
  ;     ;          ;  ;    ;  ;    ; 
  ;     ;      ;;;;;  ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ;  ;    ; 
  ;     ;     ;   ;;  ;    ;  ;    ; 
  ;      ;;;   ;;; ;  ;    ;  ;    ; 
  ;                                  
  ;                                  
  ;
  [(δmath math.tanh String)
   (δmath math.tanh Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.tanh Number)
   ,(tanh (term Number))]

  ; Default case of math functions
  [(δmath builtinserv v ...)
   (δbasic error any)

   (side-condition (string-prefix? (symbol->string (term builtinserv))
                                   "math."))

   (where any ,(string-append (symbol->string (term builtinserv))
                              ": bad argument #1 (number expected)"))
   ]

  ; to capture the "no value" error for every builtinserv 
  [(δmath builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))])

(provide δmath)