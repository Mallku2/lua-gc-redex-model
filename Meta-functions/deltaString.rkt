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
  ;                             ;                    
  ;                                                  
  ;             ;                                    
  ;             ;                                    
  ;    ;;;;   ;;;;;;   ;;;;   ;;;     ; ;;;    ;;;;; 
  ;   ;    ;    ;      ;;  ;    ;     ;;   ;  ;;  ;; 
  ;   ;         ;      ;        ;     ;    ;  ;    ; 
  ;    ;;;;     ;      ;        ;     ;    ;  ;    ; 
  ;        ;    ;      ;        ;     ;    ;  ;    ; 
  ;   ;    ;    ;      ;        ;     ;    ;  ;;  ;; 
  ;    ;;;;      ;;;   ;      ;;;;;   ;    ;   ;;; ; 
  ;                                                ; 
  ;                                            ;   ; 
  ;                                             ;;;  
  
  
  ;                                  
  ;        ;                         
  ;        ;                         
  ;        ;                         
  ;        ;                         
  ;    ;;;;;  ;    ;  ;;;;;;; ;;;;;  
  ;   ;;  ;;  ;    ;  ;  ;  ; ;;  ;; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;;  ;;  ;   ;;  ;  ;  ; ;;  ;; 
  ;    ;;;;;   ;;; ;  ;  ;  ; ;;;;;  
  ;                           ;      
  ;                           ;      
  ;                           ;

  ; PRE : {cid ∈ dom(θ)}
  [(δstring string.dump cid (osp_1 ... (cid functiondef) osp_2 ...))
   String
   
   (where String ,(str-flatten (term functiondef)))]

  [(δstring string.dump v θ)
   any
   
   (where any (δbasic error ,(string-append
                         "bad argument #1 (function expected, got "
                         (term (δbasic type v))
                         ")")))
   ]

  
  ;                          
  ;   ;;;                    
  ;     ;                    
  ;     ;                    
  ;     ;                    
  ;     ;      ;;;;   ; ;;;  
  ;     ;     ;;  ;;  ;;   ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;;;;;;  ;    ; 
  ;     ;     ;       ;    ; 
  ;     ;     ;;   ;  ;    ; 
  ;      ;;;   ;;;;   ;    ; 
  ;                          
  ;                          
  ;
  [(δstring string.len Number)
   (δstring string.len String)

   (where String (δbasic tostring Number ()))]
  
  [(δstring string.len String)
   (δbasic \# String)]
  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;    ;;;;    ;;;;   ;;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;; 
  ;    ;      ;    ;  ;    ; 
  ;    ;      ;;;;;;  ;    ; 
  ;    ;      ;       ;    ; 
  ;    ;      ;;   ;  ;;  ;; 
  ;    ;       ;;;;   ;;;;;  
  ;                   ;      
  ;                   ;      
  ;                   ;
  ; coercion
  [(δstring string.rep Number_1 Number_2 v)
   (δstring string.rep String Number_2 v)

   (where String (δbasic tostring Number_1 ()))]

  [(δstring string.rep String_1 Number_1 Number_2)
   (δstring string.rep String_1 Number_1 String_2)

   (where String_2 (δbasic tostring Number_2 ()))]

   ; negative number of reps.: returns empty string
  [(δstring string.rep String Number any)
   ""

   (side-condition (<= (term Number) 0))]

  ; {Number_1 > 0}
  ; Number_1 is not natural: the official interpreter takes floor(Number_2)
  [(δstring string.rep String Number_1 any)
   (δstring string.rep String Number_2 any)

   (where Number_2 ,(floor (term Number_1)))
   (side-condition (not (= (term Number_1) (term Number_2))))]
  
  [(δstring string.rep String 1 any)
   String]
  
  [(δstring string.rep String Number nil)
   any
   
   (where any ,(foldr (λ (str accum) (term (δbasic .. ,str ,accum)))
                      (term String)
                      (build-list
                       (- ; build-list contract: exact-nonnegative-integer?
                        (inexact->exact (term Number)) 1)
                       (λ (nmbr) (term String)))))
   ]

  ; {v_2 != nil}
  [(δstring string.rep String_1 Number String_2)
   (δbasic .. any String_1)
   
   (where any ,(foldr (λ (str accum) (term (δbasic .. (δbasic .. String_1 String_2)
                                                  ,accum)))
                      (term (δbasic .. String_1 String_2))
                      (build-list (- (term Number) 2)
                                  (λ (nmbr) (term String_1)))))
   ]

  [(δstring string.rep v_1 v_2 v_3)
   (δbasic error "string.rep: arguments of the wrong type")]
  ;                                                          
  ;                                                          
  ;                                                          
  ;                                                          
  ;                                                          
  ;    ;;;;    ;;;;   ;    ;   ;;;;    ;;;;    ;;;;    ;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;;  ;;  ;;   ;;  ;  ;    ;  ;;  ;; 
  ;    ;      ;    ;   ;  ;   ;    ;   ;      ;       ;    ; 
  ;    ;      ;;;;;;   ;  ;   ;;;;;;   ;       ;;;;   ;;;;;; 
  ;    ;      ;        ;;;;   ;        ;           ;  ;      
  ;    ;      ;;   ;    ;;    ;;   ;   ;      ;    ;  ;;   ; 
  ;    ;       ;;;;     ;;     ;;;;    ;       ;;;;    ;;;;  
  ;                                                          
  ;                                                          
  ;
  [(δstring string.reverse Number)
   (δstring string.reverse String)

   (where String (δbasic tostring Number ()))]

  [(δstring string.reverse String)
   ,(list->string (reverse (string->list (term String))))]
  
  ;                          
  ;                   ;      
  ;                   ;      
  ;                   ;      
  ;                   ;      
  ;    ;;;;   ;    ;  ;;;;;  
  ;   ;    ;  ;    ;  ;;  ;; 
  ;   ;       ;    ;  ;    ; 
  ;    ;;;;   ;    ;  ;    ; 
  ;        ;  ;    ;  ;    ; 
  ;   ;    ;  ;   ;;  ;;  ;; 
  ;    ;;;;    ;;; ;  ;;;;;  
  ;                          
  ;
  [(δstring string.sub Number_1 Number_2 Number_3)
   (δstring string.sub String Number_2 Number_3)

   (where String (δbasic tostring Number_1 ()))]
  
  ; correction of indices
  ; Number_1 < 0
  [(δstring string.sub String Number_1 Number_2)
   (δstring string.sub String 1 Number_2)

   (side-condition (< (term Number_1)
                      0))]
  
  ; If Number_2 is greater than the string length, it is corrected to that length
  [(δstring string.sub String Number_1 Number_2)
   (δstring string.sub String Number_1 (δbasic \# String))

   (side-condition (< (term (δbasic \# String))
                      (exact-floor (term Number_2))))]

  ; If, after these corrections, Number_1 is greater than Number_2, the function
  ; returns the empty string. 
  [(δstring string.sub String Number_1 Number_2)
   ""

   (side-condition (< (term Number_2)
                      (term Number_1)))]

  ; Normal case
  [(δstring string.sub String Number_1 Number_2)
   ,(substring (term String)
               (- (exact-floor (term Number_1)) 1)
               (exact-floor (term Number_2)))]

  ; to capture the "no value" error for every builtinserv 
  [(δstring builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))]

  ; Services that don't modify theta
  [(δstring builtinserv v ... θ)
   (δbasic error any)

   (side-condition (member (term builtinserv)
                           (term (; string
                                  string.dump))))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]

  ; Services that modify theta
  [(δstring builtinserv v ... θ)
   (θ (δbasic error any))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ])
  
(provide δstring)