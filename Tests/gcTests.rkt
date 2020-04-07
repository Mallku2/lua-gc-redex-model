#lang racket

(require redex
         "../grammar.rkt"
         "../Meta-functions/gc.rkt")

(define (gc-test-suite)
  ;                                                                                                  
  ;                                   ;               ;         ;     ;;;       ;                    
  ;                                   ;               ;                 ;                            
  ;                                   ;               ;                 ;               ;            
  ;                                   ;               ;                 ;               ;            
  ;    ;;;;    ;;;;     ;;;     ;;;   ; ;;;     ;;;   ;;;;;   ;;;       ;     ;;;     ;;;;;;  ;    ; 
  ;    ;;  ;  ;;  ;;   ;   ;   ;   ;  ;;   ;   ;   ;  ;;  ;;    ;       ;       ;       ;      ;   ; 
  ;    ;      ;    ;       ;  ;       ;    ;       ;  ;    ;    ;       ;       ;       ;      ;  ;  
  ;    ;      ;;;;;;   ;;;;;  ;       ;    ;   ;;;;;  ;    ;    ;       ;       ;       ;      ;  ;  
  ;    ;      ;       ;    ;  ;       ;    ;  ;    ;  ;    ;    ;       ;       ;       ;       ; ;  
  ;    ;      ;;   ;  ;   ;;   ;   ;  ;    ;  ;   ;;  ;;  ;;    ;       ;       ;       ;       ;;   
  ;    ;       ;;;;    ;;; ;    ;;;   ;    ;   ;;; ;  ;;;;;   ;;;;;      ;;;  ;;;;;      ;;;     ;   
  ;                                                                                              ;   
  ;                                                                                             ;    
  ;                                                                                            ;;    
    (test-equal (term (reach ((ref 1) ((ref 1) = 1) () ())))
                #t)
  
    (test-equal (term (reach ((ref 2)
                             ((ref 1) = 1)
                             (((ref 1) (cl 6)))
                             (((cl 6) (function x (x) ((ref 2) = 1) end))))))
                #t)
  
    (test-equal (term (reach ((ref 3)
                             ((ref 1) = 1)
                             (((ref 1) (cl 6))
                              ((ref 2) (cl 7)))
                             (((cl 6) (function x (x) ((ref 2) = 1) end))
                              ((cl 7) (function y (y) ((ref 3) = 1) end))))))
                #t)
  
  (test-equal (term (reach ((objr 1)
                           ((ref 1) = 1)
                           (((ref 1) (objr 1)))
                           (((objr 1) ((\{ \}) nil 1))))))
              #t)
  
  (test-equal (term (reach ((objr 2)
                           ((ref 1) = 1)
                           (((ref 1) (objr 1)))
                           (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))))))
              #t)
  
  (test-equal (term (reach ((objr 3)
                           ((ref 1) = 1)
                           (((ref 1) (objr 1)))
                           (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                            ((objr 2) ((\{ (\[ (objr 3) \] = 1) \}) nil 1))))))
              #t)
  
  (test-equal (term (reach ((objr 4)
                           ((ref 1) = 1)
                           (((ref 1) (objr 1)))
                           (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                            ((objr 2) ((\{ (\[ (objr 3) \] = (objr 4)) \})
                                       nil 1))))))
              #t)

  ; testing mutually recursive definitions
  (test-equal (term (reach ((objr 4)
                           ((ref 1) = 1)
                           (((ref 1) (objr 1)))
                           (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                            ((objr 2) ((\{ (\[ (objr 1) \] = 2) \}) nil 1))))))
              #f)

  (test-equal (term (reach ((objr 4)
                           ((ref 1) = 1)
                           (((ref 1) (objr 1)))
                           (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                            ((objr 2) ((\{ (\[ (objr 1) \] = 2)
                                           (\[ (objr 4) \] = 2) \}) nil 1))
                            ((objr 4) ((\{ \}) nil 1))))))
              #t)

  
  ;                                                                                                  
  ;      ;;     ;                     ;;;       ;                               ;                    
  ;     ;                               ;                                                            
  ;     ;                               ;                               ;                            
  ;     ;                               ;                               ;                            
  ;   ;;;;;   ;;;     ; ;;;     ;;;     ;     ;;;     ;;;;;;    ;;;   ;;;;;;  ;;;      ;;;;   ; ;;;  
  ;     ;       ;     ;;   ;   ;   ;    ;       ;          ;   ;   ;    ;       ;     ;;  ;;  ;;   ; 
  ;     ;       ;     ;    ;       ;    ;       ;         ;        ;    ;       ;     ;    ;  ;    ; 
  ;     ;       ;     ;    ;   ;;;;;    ;       ;       ;;     ;;;;;    ;       ;     ;    ;  ;    ; 
  ;     ;       ;     ;    ;  ;    ;    ;       ;      ;      ;    ;    ;       ;     ;    ;  ;    ; 
  ;     ;       ;     ;    ;  ;   ;;    ;       ;     ;       ;   ;;    ;       ;     ;;  ;;  ;    ; 
  ;     ;     ;;;;;   ;    ;   ;;; ;     ;;;  ;;;;;   ;;;;;;   ;;; ;     ;;;  ;;;;;    ;;;;   ;    ; 
  ;                                                                                                  
  ;                                                                                                  
  ;                                                                                                  

  ; setFin
  (test-equal (term
               (setFin (objr 1)
                        nil
                        (((objr 1) ((\{ \}) nil ⊘)))))
              (term ⊘))

  (test-equal (term
               (setFin (objr 1)
                        nil
                        (((objr 1) ((\{ \}) nil 1)))))
              (term ⊥))

  (test-equal (term
               (setFin (objr 1)
                        (objr 2)
                        (((objr 1) ((\{ \}) (objr 2) 1)))))
              (term 1))

  (test-equal (term
               (setFin (objr 1)
                        (objr 2)
                        (((objr 1) ((\{ \}) nil 1))
                         ((objr 2) ((\{ \}) nil ⊥)))))
              (term ⊥))

  ; actually mark a table for finalization
  (test-equal (term
               (setFin (objr 1)
                        (objr 2)
                        (((objr 1) ((\{ \}) nil ⊥))
                         ((objr 2) ((\{ (\[ "__gc" \] = 1) \}) nil ⊥)))))
              (term 1))

  ; marked
  (test-equal (term
               (marked (objr 1) (((objr 1) ((\{ \}) nil ⊥)))))
              (term #f))

  (test-equal (term
               (marked (objr 1) (((objr 1) ((\{ \}) nil ⊘)))))
              (term #f))

  (test-equal (term
               (marked (objr 1) (((objr 1) ((\{ \}) nil 1)))))
              (term #t))

  ; notReachFin
  (test-equal (term
               (notReachFin ((objr 2) () (((objr 1) ((\{ (\[ (objr 2) \] = 1)
                                                          \}) nil 1))
                                           ((objr 2) ((\{ \}) nil 1))))))
              (term #f))

  (test-equal (term
               (notReachFin ((objr 2) () (((objr 1) ((\{ (\[ (objr 2) \] = 1)
                                                          \}) nil ⊥))
                                           ((objr 2) ((\{ \}) nil 1))))))
              (term #t))

  (test-equal (term
               (notReachFin ((objr 2) () ())))
              (term #t))

  ; reachable from the metatable of a table marked for finalization
  (test-equal (term
               (notReachFin ((objr 3) () (((objr 1) ((\{ (\[ 1 \] = 1)
                                                          \}) (objr 2) 1))
                                           ((objr 2) ((\{ (\[ (objr 3) \] = 2)
                                                          \}) nil ⊥))))))
              (term #f))

  ; fin
  (test-equal (term (fin (objr 4)
                         ((ref 1) = 1)
                         (((ref 1) (objr 1)))
                         (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                          ((objr 2) ((\{ (\[ (objr 1) \] = 2)
                                         (\[ (objr 4) \] = 2) \}) nil 1))
                          ((objr 4) ((\{ \}) nil 1)))))
              #f)

  (test-equal (term (fin (objr 4)
                         ((ref 1) = 1)
                         (((ref 1) (objr 1)))
                         (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                          ((objr 2) ((\{ (\[ (objr 1) \] = 2) \}) nil 1))
                          ((objr 4) ((\{ \}) nil 1)))))
              #t)

  (test-equal (term (fin (objr 4)
                         ((ref 1) = 1)
                         (((ref 1) (objr 1)))
                         (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                          ((objr 2) ((\{ (\[ (objr 1) \] = 2)
                                         (\[ (objr 4) \] = 2) \}) nil 1))
                          ((objr 4) ((\{ \}) nil ⊥)))))
              #f)

  ; nextFin
  (test-equal (term (nextFin ((objr 3)
                               ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 2) \] = 1) \}) nil 1))
                                ((objr 2) ((\{ (\[ 1 \] = 2) \}) nil 2))
                                ((objr 3) ((\{ \}) nil 3))))))
              #t)

  (test-equal (term (nextFin ((objr 3)
                               ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ 1 \] = 1) \}) nil 1))
                                ((objr 2) ((\{ (\[ 1 \] = 2) \}) nil 2))
                                ((objr 3) ((\{ \}) nil 3))))))
              #t)

  (test-equal (term (nextFin ((objr 3)
                               ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ 1 \] = 1) \}) nil 2))
                                ((objr 2) ((\{ (\[ 1 \] = 2) \}) nil 4))
                                ((objr 3) ((\{ \}) nil 3))))))
              #f)

  ; cleanSigma
  (test-equal (term (cleanSigma (((ref 1) 2))
                                 ()
                                 ((ref 1) = 1)))
              (term (((ref 1) 2))))

  (test-equal (term (cleanSigma (((ref 1) 2)
                                  ((ref 2) 3))
                                 ()
                                 ((ref 1) = 1)))
              (term (((ref 1) 2))))

  ; reachable from a table marked for finalization
  (test-equal (term (cleanSigma (((ref 1) 2)
                                  ((ref 2) 3))
                                 (((objr 1) ((\{ (\[ (cl 6) \] = 1) \}) nil 1))
                                  ((cl 6) (function $1 () ((ref 2) = 1) end)))
                                 ((ref 1) = 1)))
              (term (((ref 1) 2)
                     ((ref 2) 3))))

  ; reachable from a non-reachable table, which is not marked for finalization
  (test-equal (term (cleanSigma (((ref 1) 2)
                                  ((ref 2) 3))
                                 (((objr 1) ((\{ (\[ (cl 6) \] = 1) \}) nil ⊥))
                                  ((cl 6) (function $1 () ((ref 2) = 1) end)))
                                 ((ref 1) = 1)))
              (term (((ref 1) 2))))

  ; clean theta
  ; reachable table
  (test-equal (term (cleanTheta (((ref 1) (objr 1)))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                  ((cl 1) (function $1 () \; end)))
                                 ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))) 0)))

  (test-equal (term (cleanTheta (((ref 1) (objr 1)))
                                 (((objr 1) ((\{ (\[ 2 \] = (cl 1)) \}) nil ⊥))
                                  ((cl 1) (function $1 () \; end)))
                                 ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = (cl 1)) \}) nil ⊥))
                      ((cl 1) (function $1 () \; end))) 0)))
  ; non-reachable table, no finalizer
  (test-equal (term (cleanTheta (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil 1))
                                  ((objr 2) ((\{ (\[ 2 \] = 1) \}) nil 2)))
                                 ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = 1) \}) nil 1))
                      ((objr 2) ((\{ (\[ 2 \] = 1) \}) nil 2))) (objr 2))))

  ; non-reachable table, but reachable from a table marked for finalization
  (test-equal (term (cleanTheta (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                  ((objr 2) ((\{ (\[ (objr 1) \] = 1) \})
                                             nil 2)))
                                 ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                      ((objr 2) ((\{ (\[ (objr 1) \] = 1) \}) nil 2)))
                     (objr 2))))

  ; non-reachable table, reachable from a table not marked for finalization
  (test-equal (term (cleanTheta (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                  ((objr 2) ((\{ (\[ (objr 1) \] = 1) \})
                                             nil ⊥)))
                                 ((ref 1) = 1)))
              (term (() 0)))

  (test-equal (term (cleanTheta (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                  ((objr 2) ((\{ (\[ 1 \] = 1) \})
                                             nil 2)))
                                 ((ref 1) = 1)))
              (term ((((objr 2) ((\{ (\[ 1 \] = 1) \}) nil 2)))
                     (objr 2))))

  ; gcFin
  ; no finalization
  (test-equal (term (gcFin ((ref 1) = 1)
                            (((ref 1) 2)
                             ((ref 2) 3))
                            (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                             ((objr 2) ((\{ (\[ 1 \] = 1) \})
                                        nil ⊥)))))
              (term ((((ref 1) 2)) () nil)))

  ; finalization
  (test-equal (term (gcFin ((ref 1) = 1)
                            (((ref 1) 2)
                             ((ref 2) 3))
                            (((objr 1) ((\{ \}) (objr 2) 2))
                             ((objr 2) ((\{ (\[ "__gc" \] = (cl 6)) \})
                                        nil ⊥))
                             ((objr 3) ((\{ \}) nil 1)))))
              
              (term ((((ref 1) 2))
                     (((objr 1) ((\{ \}) (objr 2) ⊘))
                      ((objr 2) ((\{ (\[ "__gc" \] = (cl 6)) \})
                                 nil ⊥))
                      ((objr 3) ((\{ \}) nil 1)))
                     ((cl 6) ((objr 1))))))

  ; check properties of returned table
  (test-equal (term (fin (objr 1) ((ref 1) = 1)
                         (((ref 1) 2)
                          ((ref 2) 3))
                         (((objr 1) ((\{ \}) (objr 2) 2))
                          ((objr 2) ((\{ (\[ "__gc" \] = (cl 6)) \}) nil ⊥))
                          ((objr 3) ((\{ \}) nil 1)))))
              #t)

  (test-equal (term (nextFin
                     ((objr 1) ((ref 1) = 1)
                               (((ref 1) 2)
                                ((ref 2) 3))
                               (((objr 1) ((\{ \}) (objr 2) 2))
                                ((objr 2) ((\{ (\[ "__gc" \] = (cl 6)) \})
                                           nil ⊥))
                                ((objr 3) ((\{ \}) nil 1))))))
              #t)

  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                              ;                                   ;       ;;;;                       
;                              ;                   ;               ;          ;                       
;                              ;                   ;               ;          ;                       
;  ;       ;   ;;;      ;;;;   ;    ;            ;;;;;;     ;;;;   ; ;;;      ;        ;;;     ;;;;;  
;  ;       ;  ;   ;    ;    ;  ;  ;;               ;       ;    ;  ;;   ;     ;       ;   ;   ;     ; 
;   ;  ;  ;  ;     ;        ;  ; ;                 ;            ;  ;     ;    ;      ;     ;  ;       
;   ;  ;  ;  ;     ;   ;;;;;;  ;;;                 ;       ;;;;;;  ;     ;    ;      ;     ;  ;;;;    
;   ; ; ; ;  ;;;;;;;  ;;    ;  ;  ;                ;      ;;    ;  ;     ;    ;      ;;;;;;;      ;;; 
;   ; ; ; ;  ;        ;     ;  ;   ;               ;      ;     ;  ;     ;    ;      ;              ; 
;    ;   ;    ;    ;  ;    ;;  ;    ;              ;      ;    ;;  ;;   ;     ;       ;    ;  ;     ; 
;    ;   ;     ;;;;    ;;;; ;  ;     ;              ;;;    ;;;; ;  ; ;;;       ;;;     ;;;;    ;;;;;  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     

  ; wk?
  (test-equal (term (wk? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                              nil ⊥)))))
              #t)

  (test-equal (term (wk? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "vk") \})
                                              nil ⊥)))))
              #t)

  (test-equal (term (wk? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "asdasdk")
                                                  \})
                                              nil ⊥)))))
              #t)

  (test-equal (term (wk? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "asdasd")
                                                  \})
                                              nil ⊥)))))
              #f)

  (test-equal (term (wk? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = 1)
                                                  \})
                                              nil ⊥)))))
              #f)

  ; wv?
  (test-equal (term (wv? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                              nil ⊥)))))
              #t)

  (test-equal (term (wv? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "kv") \})
                                              nil ⊥)))))
              #t)

  (test-equal (term (wv? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "asdasdv")
                                                  \})
                                              nil ⊥)))))
              #t)

  (test-equal (term (wv? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "asdasd")
                                                  \})
                                              nil ⊥)))))
              #f)

  (test-equal (term (wv? (objr 1) (((objr 1) ((\{ \}) (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = 1)
                                                  \})
                                              nil ⊥)))))
              #f)

  ; SO
  ; non-weak table
  (test-equal (term (SO (objr 1) (((objr 1) ((\{ (\[ (objr 1) \] = (cl 6)) \})
                                             nil ⊥)))))
              (term ((objr 1) (cl 6))))

  (test-equal (term (SO (objr 1) (((objr 1) ((\{ (\[ 1 \] = (cl 6)) \})
                                             nil ⊥)))))
              (term ((cl 6))))

  (test-equal (term (SO (objr 1) (((objr 1) ((\{ (\[ 1 \] = 2) \})
                                             nil ⊥)))))
              (term ()))

  ; weak keys
  (test-equal (term (SO (objr 1) (((objr 1) ((\{ (\[ 1 \] = (objr 2))
                                                 (\[ 2 \] = 3)
                                                 (\[ (cl 6) \] = 1)\})
                                             (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "k")
                                                  \})
                                              nil ⊥)))))
              (term ((1 (objr 2)))))

  ; weak values
  (test-equal (term (SO (objr 1) (((objr 1) ((\{ (\[ 1 \] = (objr 2))
                                                 (\[ 2 \] = 3)
                                                 (\[ (cl 6) \] = 1)\})
                                             (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "v")
                                                  \})
                                              nil ⊥)))))
              (term ((cl 6))))

  ; weak keys and values
  (test-equal (term (SO (objr 1) (((objr 1) ((\{ (\[ 1 \] = (objr 2))
                                                 (\[ 2 \] = 3)
                                                 (\[ (cl 6) \] = 1)\})
                                             (objr 2) ⊥))
                                   ((objr 2) ((\{ (\[ "__mode" \] = "kv")
                                                  \})
                                              nil ⊥)))))
              (term ()))

  ; reachTable
  ; weak keys
  (test-equal (term (reachTable (objr 3)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ 1 \] = (objr 3)) \})
                                            (objr 2) ⊥))
                                 ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                            nil ⊥)))
                                ((ref 1) = 2)))
              #t)

  (test-equal (term (reachTable (objr 3)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ (objr 1) \] = (objr 3)) \})
                                            (objr 2) ⊥))
                                 ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                            nil ⊥)))
                                ((ref 1) = 2)))
              #t)

  (test-equal (term (reachTable (objr 3)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ (objr 2) \] = (objr 3)) \})
                                            (objr 2) ⊥))
                                 ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                            nil ⊥)))
                                ((ref 1) = 2)))
              #t)

  (test-equal (term (reachTable (objr 3)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ (objr 4) \] = (objr 3)) \})
                                            (objr 2) ⊥))
                                 ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                            nil ⊥)))
                                ((ref 1) = 2)))
              #f)

  ; weak values
  (test-equal (term (reachTable (objr 3)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ (objr 1) \] = (objr 3)) \})
                                            (objr 2) ⊥))
                                 ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                            nil ⊥)))
                                ((ref 1) = 2)))
              #f)

  (test-equal (term (reachTable (cl 6)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ (cl 6) \] = (objr 3)) \})
                                            (objr 2) ⊥))
                                 ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                            nil ⊥)))
                                ((ref 1) = 2)))
              #t)

  ; non-weak table
  (test-equal (term (reachTable (objr 2)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ 1 \] = (objr 2)) \})
                                            nil ⊥)))
                                ((ref 1) = 2)))
              #t)

  ; weak keys
  (test-equal (term (reachTable (objr 2)
                                (objr 1)
                                (((ref 1) (objr 1)))
                                (((objr 1) ((\{ (\[ 1 \] = (objr 2)) \})
                                            (objr 2) ⊥))
                                 ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                           nil ⊥)))
                                ((ref 1) = 2)))
              #t)

  ; reachCte
  ; id ∈ t
  (test-equal (term (reachCte ((objr 1)
                               ((ref 1) = (objr 1))
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ \}) nil ⊥)))
                               ((ref 1) = (objr 1)))))
              #t)

  ; ∃ r ∈ t, reachCte (...)
  (test-equal (term (reachCte ((objr 1)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ \}) nil ⊥)))
                               ((ref 1) = 2))))
              #t)

  ; ∃ tid ∈ t, reachTable(...)
  ; weak keys: ephemerons
  (test-equal (term (reachCte ((objr 2)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ 1 \] = (objr 2)) \})
                                           (objr 2) ⊥))
                                ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                           nil ⊥)))
                               ((ref 1) = 2))))
              #t)

  (test-equal (term (reachCte ((objr 2)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           (objr 2) ⊥))
                                ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                           nil ⊥))
                                ((objr 3) ((\{ \}) nil ⊥)))
                               ((ref 1) = 2))))
              #t)

  (test-equal (term (reachCte ((objr 2)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           (objr 4) ⊥))
                                ((objr 4) ((\{ (\[ "__mode" \] = "k") \})
                                           nil ⊥))
                                ((objr 3) ((\{ \}) nil ⊥)))
                               ((ref 1) = 2))))
              #f)

  ; weak values
  (test-equal (term (reachCte ((objr 2)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           (objr 4) ⊥))
                                ((objr 4) ((\{ (\[ "__mode" \] = "v") \})
                                           nil ⊥))
                                ((objr 3) ((\{ \}) nil ⊥)))
                               ((ref 1) = 2))))
              #f)

  (test-equal (term (reachCte ((objr 3)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           (objr 4) ⊥))
                                ((objr 4) ((\{ (\[ "__mode" \] = "v") \})
                                           nil ⊥))
                                ((objr 3) ((\{ \}) nil ⊥)))
                               ((ref 1) = 2))))
              #t)

  ; weak keys and values
  (test-equal (term (reachCte ((objr 3)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           (objr 4) ⊥))
                                ((objr 4) ((\{ (\[ "__mode" \] = "kv") \})
                                           nil ⊥))
                                ((objr 3) ((\{ \}) nil ⊥)))
                               ((ref 1) = 2))))
              #f)

  (test-equal (term (reachCte ((objr 2)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           (objr 4) ⊥))
                                ((objr 4) ((\{ (\[ "__mode" \] = "kv") \})
                                           nil ⊥))
                                ((objr 3) ((\{ \}) nil ⊥)))
                               ((ref 1) = 2))))
              #f)

  ; strong tables
  (test-equal (term (reachCte ((objr 2)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           nil ⊥)))
                               ((ref 1) = 2))))
              #t)

  (test-equal (term (reachCte ((objr 3)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = (objr 2)) \})
                                           nil ⊥)))
                               ((ref 1) = 2))))
              #t)

  ; ∃ cid ∈ t, reachTable(...)
  (test-equal (term (reachCte ((objr 2)
                               ((ref 1) = 2)
                               (((ref 1) (objr 1))
                                ((ref 2) (objr 2)))
                               (((objr 1) ((\{ (\[ (cl 6) \] = (objr 2)) \})
                                           nil ⊥))
                                ((cl 6) (function $1 () ((ref 2) = 1) end)))
                               ((ref 1) = 2))))
              #t)

  ; cleanWeakTables
  ; weak values
  (test-equal (term (cleanWeakTables (((ref 1) (objr 1)))
                                     (((objr 1) ((\{ (\[ 1 \] = (objr 3))
                                                     (\[ 2 \] = 2)
                                                     (\[ (cl 6) \] = (objr 4))
                                                     \})
                                                 (objr 2) ⊥))
                                      ((objr 4) ((\{ (\[ 1 \] = (objr 3))
                                                     (\[ 2 \] = 2)
                                                     (\[ (cl 6) \] = 4) \})
                                                 (objr 2) ⊥))
                                      ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                                 nil ⊥))
                                      ((objr 3) ((\{ \}) nil ⊥))
                                      ((cl 6) (function $1 () \; end)))
                                     ((ref 1) = 2)))
              (term (((objr 1) ((\{ (\[ 2 \] = 2) \}) (objr 2) ⊥))
                     ((objr 4) ((\{ (\[ 2 \] = 2)
                                    (\[ (cl 6) \] = 4) \})
                                (objr 2) ⊥))
                     ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                nil ⊥))
                     ((objr 3) ((\{ \}) nil ⊥))
                     ((cl 6) (function $1 () \; end)))))

  (test-equal (term (cleanWeakTables (((ref 1) (objr 1)))
                                     (((objr 1) ((\{ (\[ 1 \] = (objr 2)) \})
                                                 (objr 2) ⊥))
                                      ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                                 nil ⊥)))
                                     ((ref 1) = 2)))
              (term (((objr 1) ((\{ (\[ 1 \] = (objr 2)) \}) (objr 2) ⊥))
                     ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                nil ⊥)))))

  ; weak keys
  (test-equal (term (cleanWeakTables (((ref 1) (objr 1)))
                                     (((objr 1) ((\{ (\[ (objr 4) \] =
                                                         (objr 2))
                                                     (\[ (objr 5) \] =
                                                         (objr 2))\})
                                                 (objr 2) ⊥))
                                      ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                                 nil ⊥))
                                      ((objr 4) ((\{ \})
                                                 (objr 2) 1))
                                      ((objr 5) ((\{ \})
                                                 (objr 2) ⊥)))
                                     ((ref 1) = 2)))
              (term (((objr 1) ((\{ (\[ (objr 4) \] = (objr 2)) \})
                                (objr 2) ⊥))
                     ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                nil ⊥))
                     ((objr 4) ((\{ \})
                                (objr 2) 1))
                     ((objr 5) ((\{ \})
                                (objr 2) ⊥)))))

  ; weak keys and values
  (test-equal (term (cleanWeakTables (((ref 1) (objr 1)))
                                     (((objr 1) ((\{ (\[ (objr 4) \] =
                                                         (objr 2))
                                                     (\[ (objr 5) \] =
                                                         (objr 2))\})
                                                 (objr 2) ⊥))
                                      ((objr 2) ((\{ (\[ "__mode" \] = "vk") \})
                                                 nil ⊥))
                                      ((objr 4) ((\{ \})
                                                 (objr 2) 1))
                                      ((objr 5) ((\{ \})
                                                 (objr 2) ⊥)))
                                     ((ref 1) = 2)))
              (term (((objr 1) ((\{ (\[ (objr 4) \] = (objr 2)) \})
                                (objr 2) ⊥))
                     ((objr 2) ((\{ (\[ "__mode" \] = "vk") \})
                                nil ⊥))
                     ((objr 4) ((\{ \})
                                (objr 2) 1))
                     ((objr 5) ((\{ \})
                                (objr 2) ⊥)))))

  ; interaction finalization-weak tables: (objr 4) is marked for finalization
  (test-equal (term (cleanWeakTables (((ref 1) (objr 1)))
                                     (((objr 1) ((\{ (\[ (objr 4) \] =
                                                         (objr 4)) \})
                                                 (objr 2) 1))
                                      ((objr 2) ((\{ (\[ "__mode" \] = "k")
                                                     (\[ "__gc" \] = (cl 6)) \})
                                                 nil 1))
                                      ((objr 4) ((\{ \}) (objr 2) 1))
                                      ((cl 6) (function $1 () ((ref 1) = 1)
                                                        end)))
                                     ((ref 1) = 1)))
              (term (((objr 1)
                      ((|{| (|[| (objr 4) |]| = (objr 4)) |}|) (objr 2) 1))
                     ((objr 2)
                      ((|{|
                        (|[| "__mode" |]| = "k")
                        (|[| "__gc" |]| = (cl 6))
                        |}|)
                       nil
                       1))
                     ((objr 4) ((|{| |}|) (objr 2) 1))
                     ((cl 6) (function $1 () ((ref 1) = 1) end)))))

  ; (objr 4) is not marked for finalization
  (test-equal (term (cleanWeakTables (((ref 1) (objr 1)))
                                     (((objr 1) ((\{ (\[ (objr 4) \] =
                                                         (objr 4)) \})
                                                 (objr 2) 1))
                                      ((objr 2) ((\{ (\[ "__mode" \] = "k")
                                                     (\[ "__gc" \] = (cl 6)) \})
                                                 nil 1))
                                      ((objr 4) ((\{ \}) (objr 2) ⊘))
                                      ((cl 6) (function $1 () ((ref 1) = 1)
                                                        end)))
                                     ((ref 1) = 1)))
              (term (((objr 1)
                      ((|{| |}|) (objr 2) 1))
                     ((objr 2)
                      ((|{|
                        (|[| "__mode" |]| = "k")
                        (|[| "__gc" |]| = (cl 6))
                        |}|)
                       nil
                       1))
                     ((objr 4) ((|{| |}|) (objr 2) ⊘))
                     ((cl 6) (function $1 () ((ref 1) = 1) end)))))

  ; finWeak
  ; check weak tables' semantics in finalization
  (test-equal (term (finWeak (objr 4)
                             ((ref 1) = 1)
                             (((ref 1) (objr 1)))
                             (((objr 1) ((\{ (\[ (objr 4) \] = 1) \})
                                         (objr 2) 1))
                              ((objr 2) ((\{ (\[ "__mode" \] = "k") \}) nil 1))
                              ((objr 4) ((\{ \}) nil 2)))))
              #t)

  
  (test-equal (term (finWeak (objr 4)
                             ((ref 1) = 1)
                             (((ref 1) (objr 1)))
                             (((objr 1) ((\{ (\[ (objr 4) \] = (objr 4)) \})
                                         (objr 2) 1))
                              ((objr 2) ((\{ (\[ "__mode" \] = "k") \}) nil 1))
                              ((objr 4) ((\{ \}) nil 2)))))
              #t)

  (test-equal (term (finWeak (objr 4)
                             ((ref 1) = 1)
                             (((ref 1) (objr 1)))
                             (((objr 1) ((\{ (\[ (objr 4) \] = (objr 4)) \})
                                         (objr 2) 1))
                              ((objr 2) ((\{ (\[ "__mode" \] = "k")
                                             (\[ "__gc" \] = (cl 6)) \}) nil 1))
                              ((objr 4) ((\{ \}) (objr 2) 2))
                              ((cl 6) (function $1 () ((ref 1) = 1) end)))))
              #t)

  ; notFinVal
  (test-equal (term (notFinVal (objr 4)
                               (((objr 1) ((\{ (\[ 1 \] = (objr 4)) \})
                                           (objr 2) 1))
                                ((objr 2) ((\{ (\[ "__mode" \] = "k") \}) nil
                                                                           1))
                                ((objr 4) ((\{ \}) (objr 2) 2)))))
              #f)

  (test-equal (term (notFinVal (objr 4)
                               (((objr 1) ((\{ (\[ 1 \] = (objr 4)) \})
                                           (objr 2) 1))
                                ((objr 2) ((\{ (\[ "__mode" \] = "v") \}) nil
                                                                           1))
                                ((objr 4) ((\{ \}) (objr 2) 2)))))
              #f)

  (test-equal (term (notFinVal (objr 4)
                               (((objr 2) ((\{ (\[ "__mode" \] = "v") \}) nil
                                                                           1))
                                ((objr 4) ((\{ \}) (objr 2) 2))
                                ((objr 1) ((\{ (\[ 1 \] = (objr 4)) \})
                                           (objr 2) 1))
                                )))
              #f)

  (test-equal (term (notFinVal (objr 4)
                               (((objr 2) ((\{ (\[ "__mode" \] = 1) \}) nil
                                                                        1))
                                ((objr 4) ((\{ \}) (objr 2) 2))
                                ((objr 1) ((\{ (\[ 1 \] = (objr 4)) \})
                                           (objr 2) 1))
                                )))
              #t)

  ; cleanThetaWeak
  ; check if previous semantics is preserved
  ; reachable table
  (test-equal (term (cleanThetaWeak (((ref 1) (objr 1)))
                                    (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                     ((cl 1) (function $1 () \; end)))
                                    ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))) 0)))

  (test-equal (term (cleanThetaWeak (((ref 1) (objr 1)))
                                 (((objr 1) ((\{ (\[ 2 \] = (cl 1)) \}) nil ⊥))
                                  ((cl 1) (function $1 () \; end)))
                                 ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = (cl 1)) \}) nil ⊥))
                      ((cl 1) (function $1 () \; end))) 0)))
  ; non-reachable table, no finalizer
  (test-equal (term (cleanThetaWeak (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil 1))
                                  ((objr 2) ((\{ (\[ 2 \] = 1) \}) nil 2)))
                                 ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = 1) \}) nil 1))
                      ((objr 2) ((\{ (\[ 2 \] = 1) \}) nil 2))) (objr 2))))

  ; non-reachable table, but reachable from a table marked for finalization
  (test-equal (term (cleanThetaWeak (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                  ((objr 2) ((\{ (\[ (objr 1) \] = 1) \})
                                             nil 2)))
                                 ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                      ((objr 2) ((\{ (\[ (objr 1) \] = 1) \}) nil 2)))
                     (objr 2))))

  ; non-reachable table, reachable from a table not marked for finalization
  (test-equal (term (cleanThetaWeak (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                  ((objr 2) ((\{ (\[ (objr 1) \] = 1) \})
                                             nil ⊥)))
                                 ((ref 1) = 1)))
              (term (() 0)))

  (test-equal (term (cleanThetaWeak (((ref 1) 2))
                                 (((objr 1) ((\{ (\[ 2 \] = 1) \}) nil ⊥))
                                  ((objr 2) ((\{ (\[ 1 \] = 1) \})
                                             nil 2)))
                                 ((ref 1) = 1)))
              (term ((((objr 2) ((\{ (\[ 1 \] = 1) \}) nil 2)))
                     (objr 2))))

  ; check if weak tables semantics is taken into account
  ; (objr 3) appears as a value of a weak table
  (test-equal (term (cleanThetaWeak (((ref 1) 2))
                                    (((objr 1) ((\{ (\[ 2 \] = (objr 3)) \})
                                                (objr 2)
                                                ⊥))
                                     ((objr 2) ((\{ (\[ "__mode" \] = "v")\})
                                                nil 2))
                                     ((objr 3) ((\{ \})
                                                (objr 2) 3)))
                                    ((ref 1) = 1)))
              (term ((((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                 nil 2))
                      ((objr 3) ((\{ \})
                                 (objr 2) 3)))
                     (objr 2))))

  ; now (objr 3) appears as a key, and has the highest priority for finalization
  (test-equal (term (cleanThetaWeak (((ref 1) 2))
                                    (((objr 1) ((\{ (\[ (objr 3) \] = 2) \})
                                                (objr 2)
                                                ⊥))
                                     ((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                                nil 2))
                                     ((objr 3) ((\{ \})
                                                (objr 2) 3)))
                                    ((ref 1) = 1)))
              (term ((((objr 2) ((\{ (\[ "__mode" \] = "v") \})
                                 nil 2))
                      ((objr 3) ((\{ \})
                                 (objr 2) 3)))
                     (objr 3))))

  ; now finalization takes into account semantics of weak tables
  (test-equal (term (cleanThetaWeak (((ref 1) (objr 1)))
                                    (((objr 1) ((\{ (\[ (objr 3) \] = 2) \})
                                                (objr 2)
                                                4))
                                     ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                                nil 2))
                                     ((objr 3) ((\{ \})
                                                (objr 2) 3)))
                                    ((ref 1) = 1)))
              (term ((((objr 1) ((\{ (\[ (objr 3) \] = 2) \})
                                                (objr 2)
                                                4))
                      ((objr 2) ((\{ (\[ "__mode" \] = "k") \})
                                 nil 2))
                      ((objr 3) ((\{ \})
                                 (objr 2) 3)))
                     (objr 3))))

  ; gcFinWeakAware
  ; tables are chosen for finalization taking into account reachCte
  (test-equal (term (gcFinWeakAware ((ref 1) = 1)
                                    (((ref 1) (objr 1)))
                                    (((objr 1) ((\{ (\[ (objr 3) \] = 2) \})
                                                (objr 2)
                                                4))
                                     ((objr 2) ((\{ (\[ "__mode" \] = "k")
                                                    (\[ "__gc" \] = (cl 1))\})
                                                nil 2))
                                     ((objr 3) ((\{ \})
                                                (objr 2) 3))
                                     ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| (|[| (objr 3) |]| = 2) |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "k")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((objr 3) ((|{| |}|) (objr 2) 3))
                      ((cl 1) (function $1 () |;| end)))
                     ((cl 1) ((objr 3))))))

  (test-equal (term (gcFinWeakAware ((ref 1) = 1)
                                    (((ref 1) (objr 1)))
                                    (((objr 1) ((\{ (\[ (objr 3) \] = 2) \})
                                                (objr 2)
                                                4))
                                     ((objr 2) ((\{ (\[ "__mode" \] = "v")
                                                    (\[ "__gc" \] = (cl 1))\})
                                                nil 2))
                                     ((objr 3) ((\{ \})
                                                (objr 2) 3))
                                     ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| (|[| (objr 3) |]| = 2) |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "v")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((objr 3) ((|{| |}|) (objr 2) 3))
                      ((cl 1) (function $1 () |;| end)))
                     nil)))
  ; tables cannot be finalized if present as values from a weak table: they
  ; must be removed from the weak table, first
  (test-equal (term (gcFinWeakAware ((ref 1) = 1)
                                    (((ref 1) (objr 1)))
                                    (((objr 1) ((\{ (\[ 1 \] = (objr 3)) \})
                                                (objr 2)
                                                4))
                                     ((objr 2) ((\{ (\[ "__mode" \] = "v")
                                                    (\[ "__gc" \] = (cl 1))\})
                                                nil 2))
                                     ((objr 3) ((\{ \})
                                                (objr 2) 3))
                                     ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| (|[| 1 |]| = (objr 3)) |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "v")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((objr 3) ((|{| |}|) (objr 2) 3))
                      ((cl 1) (function $1 () |;| end)))
                     nil)))

  ; gcFinWeak
  ; a table marked for finalization, that appears as a value from a weak table,
  ; must be removed from the weak table, first
  (test-equal (term (gcFinWeak ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ 1 \] = (objr 3)) \})
                                           (objr 2)
                                           4))
                                ((objr 2) ((\{ (\[ "__mode" \] = "v")
                                               (\[ "__gc" \] = (cl 1))\})
                                           nil 2))
                                ((objr 3) ((\{ \})
                                           (objr 2) 3))
                                ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "v")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((objr 3) ((|{| |}|) (objr 2) 3))
                      ((cl 1) (function $1 () |;| end)))
                     nil)))

  ; now, it can be finalized
  (test-equal (term (gcFinWeak ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ \})
                                           (objr 2)
                                           4))
                                ((objr 2) ((\{ (\[ "__mode" \] = "v")
                                               (\[ "__gc" \] = (cl 1))\})
                                           nil 2))
                                ((objr 3) ((\{ \})
                                           (objr 2) 3))
                                ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "v")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((objr 3) ((|{| |}|) (objr 2) ⊘))
                      ((cl 1) (function $1 () |;| end)))
                     ((cl 1) ((objr 3))))))

  ; now, it can be removed from θ
  (test-equal (term (gcFinWeak ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ \})
                                           (objr 2)
                                           4))
                                ((objr 2) ((\{ (\[ "__mode" \] = "v")
                                               (\[ "__gc" \] = (cl 1))\})
                                           nil 2))
                                ((objr 3) ((\{ \})
                                           (objr 2) ⊘))
                                ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "v")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((cl 1) (function $1 () |;| end)))
                     nil)))

  (test-equal (term (gcFinWeak ((ref 1) = 1)
                               (((ref 1) 1))
                               (((objr 1) ((\{ \})
                                           (objr 2)
                                           4))
                                ((objr 2) ((\{ (\[ "__mode" \] = "v")
                                               (\[ "__gc" \] = (cl 1))\})
                                           nil 2))
                                ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) 1))
                     (((objr 1)
                       ((|{| |}|) (objr 2) ⊘))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "v")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((cl 1) (function $1 () |;| end)))
                     ((cl 1) ((objr 1))))))

  ; keys from weak tables are, first, finalized
  
  (test-equal (term (gcFinWeak ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = 1) \})
                                           (objr 2)
                                           4))
                                ((objr 2) ((\{ (\[ "__mode" \] = "k")
                                               (\[ "__gc" \] = (cl 1))\})
                                           nil 2))
                                ((objr 3) ((\{ \})
                                           (objr 2) 3))
                                ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| (\[ (objr 3) \] = 1) |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "k")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((objr 3) ((|{| |}|) (objr 2) ⊘))
                      ((cl 1) (function $1 () |;| end)))
                     ((cl 1) ((objr 3))))))

  ; then, it can be removed
  (test-equal (term (gcFinWeak ((ref 1) = 1)
                               (((ref 1) (objr 1)))
                               (((objr 1) ((\{ (\[ (objr 3) \] = 1) \})
                                           (objr 2)
                                           4))
                                ((objr 2) ((\{ (\[ "__mode" \] = "k")
                                               (\[ "__gc" \] = (cl 1))\})
                                           nil 2))
                                ((objr 3) ((\{ \})
                                           (objr 2) ⊘))
                                ((cl 1) (function $1 () \; end)))))
              (term ((((ref 1) (objr 1)))
                     (((objr 1)
                       ((|{| |}|) (objr 2) 4))
                      ((objr 2)
                       ((|{|
                         (|[| "__mode" |]| = "k")
                         (|[| "__gc" |]| = (cl 1))
                         |}|)
                        nil
                        2))
                      ((objr 3) ((|{| |}|) (objr 2) ⊘))
                      ((cl 1) (function $1 () |;| end)))
                     nil)))


  (test-results))

(provide gc-test-suite)