#lang racket
(require redex
         math/flonum ; operations over flonums
         "../grammar.rkt"
         "./deltaBasic.rkt")


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

   (where Number_2 ,(flacos (real->double-flonum (term Number_1))))]
  
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

   (where Number_2 ,(flasin (real->double-flonum (term Number_1))))]
  
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
   ,(flatan (real->double-flonum (term Number)))]

  
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
   ,(flceiling (real->double-flonum (term Number)))]
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
   ,(flcos (real->double-flonum (term Number)))]

  
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
   ,(flexp (real->double-flonum (term Number)))]

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
  
  [(δmath math.floor Number)
   ,(flfloor (real->double-flonum (term Number)))]
  
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
  
  ; from ref. manual: "returns the remainder of the division of Number_1 by
  ; Number_2 that rounds the quotient towards zero"
  [(δmath math.fmod Number_1 Number_2)
   (δbasic -
           Number_1
           (δbasic *
                   ; rounding towards zero
                   (δbasic *
                           ,(sgn (term Number_3))
                           (δmath math.floor
                                  (δmath math.abs Number_3)))
                   Number_2))
   
   (where Number_3 (δbasic / Number_1 Number_2))]
  
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
  ; coercion
  [(δmath math.log Number_1 ... String v ...)
   (δmath math.log Number_1 ... Number_2 v ...)
   
   (where Number_2 (δbasic tonumber String nil))]

  ; base is optional
  [(δmath math.log Number_1 nil)
   ,(fllog (term Number_2))

   (where Number_2 ,(real->double-flonum (term Number_1)))]

  [(δmath math.log Number_1 Number_2)
   ,(fllogb (real->double-flonum (term Number_2))
            (real->double-flonum (term Number_1)))]

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
  
  [(δmath math.modf Number_1)
   (< Number_2  Number_3 >)

   (where Number_2 ,(truncate (term Number_1)))
   (where Number_3 (δbasic - Number_1 Number_2))]
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
   ,(flsin (real->double-flonum (term Number)))]
  
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
   ,(flsqrt (real->double-flonum (term Number)))]
  
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
   ,(fltan (real->double-flonum (term Number)))]
  
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

  ; default case of math functions
  [(δmath builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append (symbol->string (term builtinserv))
                              ": bad argument #1 (number expected)"))
   ]
  )

(provide δmath)