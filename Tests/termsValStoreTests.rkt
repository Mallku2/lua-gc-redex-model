#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/termsValStore.rkt")

(define (terms-val-store-test-suite)
  ; Implicit dereferencing
  (test-->> terms-val-store
            (term ((((ref 1) 2)) : (ref 1)))
            (term ((((ref 1) 2)) : 2)))
  
  (test-->> terms-val-store
            (term ((((ref 1) 1)
                    ((ref 2) 2)
                    ((ref 3) 3)
                    ((ref 4) 4)) : (ref 3)))
            (term ((((ref 1) 1)
                    ((ref 2) 2)
                    ((ref 3) 3)
                    ((ref 4) 4)) : 3)))
  
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
  ; Ordinary variable assignment
  (test-->> terms-val-store
            (term ((((ref 1) 1) ((ref 2) 2)) : ((ref 1) = 3)))
  
            (term ((((ref 1) 3) ((ref 2) 2)) : \;)))

  ; Local statement
  (test-->> terms-val-store
            (term (() : 
                      (local X = 1 in ($statFunCall X ()) end)))
            
            (term ((((ref 1) 1)) :  (($statFunCall (ref 1) ())
                                     ((rEnv (ref 1))) LocalBody))))

  (test-->> terms-val-store
            (term (() : 
                      (local X Y = 1 nil in ($statFunCall X ()) end)))
            (term ((((ref 1) 1) ((ref 2) nil)) : (($statFunCall (ref 1) ())
                                                  ((rEnv (ref 1))
                                                   (rEnv (ref 2)))
                                                  LocalBody))))


  (test-results))

(provide terms-val-store-test-suite)
