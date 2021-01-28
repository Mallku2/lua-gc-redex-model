#lang racket
(require redex
         "../Meta-functions/objStoreMetaFunctions.rkt")

; "black-box testing"

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
              (term (objr ,objStoreFirstLocation)))
              
  (test-equal (term (freshObjRef (((objr ,objStoreFirstLocation) ((\{ \}) nil 1)))))
              (term (objr ,(add1 objStoreFirstLocation))))
  
  (test-results))


(define (test-all-obj-store-metafunctions-suites)
  (refBelongsToTheta-test-suite)
  (freshObjRef-test-suite)
  )
;
(provide test-all-obj-store-metafunctions-suites)