#lang racket
(require redex
         "../Meta-functions/objStoreMetafunctions.rkt")

; "black-box testing"


;                                                                                  
;                                                                                  
;                                                                                  
;                                      ;;;          ;                              
;   ;;;;                              ;    ;;;;;;;  ;                 ;            
;   ;   ;                             ;       ;     ;                 ;            
;   ;   ;;   ;;;;    ; ;;;   ;;;;   ;;;;;;    ;     ; ;;;    ;;;;   ;;;;;;   ;;;;  
;   ;    ;  ;;  ;;   ;;     ;;  ;;    ;       ;     ;;   ;  ;;  ;;    ;          ; 
;   ;    ;  ;    ;   ;      ;    ;    ;       ;     ;    ;  ;    ;    ;          ; 
;   ;    ;  ;;;;;;   ;      ;;;;;;    ;       ;     ;    ;  ;;;;;;    ;      ;;;;; 
;   ;   ;;  ;        ;      ;         ;       ;     ;    ;  ;         ;     ;    ; 
;   ;   ;   ;;       ;      ;;        ;       ;     ;    ;  ;;        ;     ;   ;; 
;   ;;;;     ;;;;;   ;       ;;;;;    ;       ;     ;    ;   ;;;;;     ;;;   ;;; ; 
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
(define (derefTheta-test-suite)
  (test-equal (term (derefTheta (((objr 1) ((\{ \}) nil 1))) (objr 1)))
              (term ((\{ \}) nil 1)))
  
  (test-equal (term (derefTheta (((objr 1) ((\{ \}) nil 1))
                                 ((objr 2) ((\{ (\[ 1 \] = 1) \}) nil 1))
                                 ((objr 3) ((\{ (\[ 2 \] = 2) \}) nil 1))) (objr 2)))
              (term ((\{ (\[ 1 \] = 1) \}) nil 1)))
  
  (test-results))



;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                      ;;;                   ;;;                                                            ;                              
;                     ;     ;;;;;              ;                                   ;;;;;;;         ;;;;;;;  ;                 ;            
;                     ;     ;    ;             ;                                      ;               ;     ;                 ;            
;    ; ;;;   ;;;;   ;;;;;;  ;    ;   ;;;;      ;     ;;;;   ; ;;;    ;;;;;;  ;;;;;    ;      ;;;;     ;     ; ;;;    ;;;;   ;;;;;;   ;;;;  
;    ;;     ;;  ;;    ;     ;   ;;  ;;  ;;     ;    ;;  ;;  ;;   ;  ;    ;  ;         ;     ;;  ;;    ;     ;;   ;  ;;  ;;    ;          ; 
;    ;      ;    ;    ;     ;;;;    ;    ;     ;    ;    ;  ;    ;  ;    ;  ;;        ;     ;    ;    ;     ;    ;  ;    ;    ;          ; 
;    ;      ;;;;;;    ;     ;   ;;  ;;;;;;     ;    ;    ;  ;    ;  ;    ;    ;;;     ;     ;    ;    ;     ;    ;  ;;;;;;    ;      ;;;;; 
;    ;      ;         ;     ;    ;  ;          ;    ;    ;  ;    ;   ;;;;        ;    ;     ;    ;    ;     ;    ;  ;         ;     ;    ; 
;    ;      ;;        ;     ;   ;;  ;;         ;    ;;  ;;  ;    ;  ;            ;    ;     ;;  ;;    ;     ;    ;  ;;        ;     ;   ;; 
;    ;       ;;;;;    ;     ;;;;;    ;;;;;   ;;;;;   ;;;;   ;    ;   ;;;;;  ;;;;;     ;      ;;;;     ;     ;    ;   ;;;;;     ;;;   ;;; ; 
;                                                                   ;     ;                                                                
;                                                                   ;     ;                                                                
;                                                                    ;;;;;                                                                 
;                                                                                                                                          

(define (refBelongsToTheta-test-suite)
  (test-equal (term (refBelongsToTheta? (objr 1) (((objr 1) ((\{ \}) nil 1)))))
              (term #t))
  
  (test-equal (term (refBelongsToTheta? (objr 2) (((objr 1) ((\{ \}) nil 1)))))
              (term #f))
  
  (test-results))


;;                                                                                          
;;                                                                                          
;;                                                                                          
;;      ;;;                          ;               ;           ;                      ;;; 
;;     ;                             ;        ;;;;   ;               ;;;;;             ;    
;;     ;                             ;        ;  ;   ;               ;   ;;            ;    
;;   ;;;;;;   ; ;;;   ;;;;    ;;;;;  ; ;;;   ;    ;  ; ;;;    ;;;;   ;    ;   ;;;;   ;;;;;; 
;;     ;      ;;     ;;  ;;  ;       ;;   ;  ;    ;  ;;  ;;      ;   ;   ;;  ;;  ;;    ;    
;;     ;      ;      ;    ;  ;;      ;    ;  ;    ;  ;    ;      ;   ;;;;    ;    ;    ;    
;;     ;      ;      ;;;;;;    ;;;   ;    ;  ;    ;  ;    ;      ;   ;  ;    ;;;;;;    ;    
;;     ;      ;      ;            ;  ;    ;  ;    ;  ;    ;      ;   ;   ;   ;         ;    
;;     ;      ;      ;;           ;  ;    ;   ;  ;   ;;  ;;      ;   ;    ;  ;;        ;    
;;     ;      ;       ;;;;;  ;;;;;   ;    ;   ;;;;   ; ;;;       ;   ;    ;;  ;;;;;    ;    
;;                                                               ;                          
;;                                                               ;                          
;;                                                           ;;;;                           
;;                                                                                          

(define (freshObjRef-test-suite)
  (test-equal (term (freshObjRef ()))
              (term (objr 6)))
              
  (test-equal (term (freshObjRef (((objr 6) ((\{ \}) nil 1)))))
              (term (objr 7)))
  
  (test-results))



;                                                                  
;                                   ;                              
;                                   ;                              
;                          ;;;;;;;  ;                 ;            
;                             ;     ;                 ;            
;    ;;;;;   ;;;;   ;;;;;;;   ;     ; ;;;    ;;;;   ;;;;;;    ;;;  
;   ;;  ;;  ;;  ;;  ;  ;  ;   ;     ;;   ;  ;;  ;;    ;      ;   ; 
;   ;    ;  ;    ;  ;  ;  ;   ;     ;    ;  ;    ;    ;          ; 
;   ;    ;  ;    ;  ;  ;  ;   ;     ;    ;  ;;;;;;    ;      ;;;;; 
;   ;    ;  ;    ;  ;  ;  ;   ;     ;    ;  ;         ;     ;    ; 
;   ;;  ;;  ;;  ;;  ;  ;  ;   ;     ;    ;  ;;   ;    ;     ;   ;; 
;    ;;; ;   ;;;;   ;  ;  ;   ;     ;    ;   ;;;;      ;;;   ;;; ; 
;        ;                                                         
;    ;   ;                                                         
;     ;;;                                                          

(define (domTheta-test-suite)
  (test-equal (term (domTheta (((objr 1) ((\{ \}) nil 1)))))
              (term ((objr 1))))

  (test-equal (term (domTheta (((objr 1) ((\{ \}) nil 1))
                              ((objr 2) ((\{ \}) nil 1)))))
              (term ((objr 1)
                     (objr 2))))

  (test-equal (term (domTheta ()))
              (term ()))
  
  (test-results))

(define (test-all-obj-store-metafunctions-suites)
  (derefTheta-test-suite)
  (refBelongsToTheta-test-suite)
  (freshObjRef-test-suite)
  (domTheta-test-suite)
  )
;
(provide test-all-obj-store-metafunctions-suites)