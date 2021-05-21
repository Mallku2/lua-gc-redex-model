#lang racket
(require redex
         math/base
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
  [(δmath math.abs v_1 v_2 v_3 ...)
   (δmath math.abs v_1)]
  
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
  [(δmath math.acos v_1 v_2 v_3 ...)
   (δmath math.acos v_1)]
  
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
  [(δmath math.asin v_1 v_2 v_3 ...)
   (δmath math.asin v_1)]
  
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
  [(δmath math.atan v_1 v_2 v_3 ...)
   (δmath math.atan v_1)]
  
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
  [(δmath math.ceil v_1 v_2 v_3 ...)
   (δmath math.ceil v_1)]
  
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
  [(δmath math.cos v_1 v_2 v_3 ...)
   (δmath math.cos v_1)]
  
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
  [(δmath math.cosh v_1 v_2 v_3 ...)
   (δmath math.cosh v_1)]
  
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
  [(δmath math.deg v_1 v_2 v_3 ...)
   (δmath math.deg v_1)]
  
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
  [(δmath math.exp v_1 v_2 v_3 ...)
   (δmath math.exp v_1)]
  
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
  [(δmath math.floor v_1 v_2 v_3 ...)
   (δmath math.floor v_1)]
  
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
  [(δmath math.fmod v_1 v_2 v_3 v_4 ...)
   (δmath math.fmod v_1 v_2)]
  
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
  ; default for base is e
  [(δmath math.log v_1)
   (δmath math.log v_1 ,euler.0)]
  
  [(δmath math.log v_1 v_2 v_3 v_4 ...)
   (δmath math.log v_1 v_2)]
  
  ; coercion
  [(δmath math.log Number_1 ... String v ...)
   (δmath math.log Number_1 ... Number_2 v ...)
   
   (where Number_2 (δbasic tonumber String nil))]

  [(δmath math.log Number_1 v)
   ,(fllog (term Number_2))

   ; or returns its value into a parenthesized expression
   (where (\( true \)) (δbasic or
                               (δbasic == v ,euler.0)
                               (δbasic == v nil)))
   
   (where Number_2 ,(real->double-flonum (term Number_1)))]

  ; {Number_2 ≠ euler.0}
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
  
  [(δmath math.max Number_1 Number_2 ...)
   ,(foldr (λ (nmbr accum) (max nmbr accum))
           -inf.0
           (term (Number_1 Number_2 ...)))]
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
  [(δmath math.modf v_1 v_2 v_3 ...)
   (δmath math.modf v_1)]
  
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
  [(δmath math.rad v_1 v_2 v_3 ...)
   (δmath math.rad v_1)]
  
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
  [(δmath math.sin v_1 v_2 v_3 ...)
   (δmath math.sin v_1)]
  
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
  [(δmath math.sinh v_1 v_2 v_3 ...)
   (δmath math.sinh v_1)]
  
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
  [(δmath math.sqrt v_1 v_2 v_3 ...)
   (δmath math.sqrt v_1)]
  
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
  [(δmath math.tan v_1 v_2 v_3 ...)
   (δmath math.tan v_1)]
  
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
  [(δmath math.tanh v_1 v_2 v_3 ...)
   (δmath math.tanh v_1)]
  
  [(δmath math.tanh String)
   (δmath math.tanh Number)
   
   (where Number (δbasic tonumber String nil))]
  
  [(δmath math.tanh Number)
   ,(tanh (term Number))]

  ; default case of math functions
  [(δmath builtinserv any ...)
   (δbasic error String)

   (where String ,(string-append "erroneous actual parameters to "
                                 (symbol->string (term builtinserv))))
   ]
  )

(provide δmath)