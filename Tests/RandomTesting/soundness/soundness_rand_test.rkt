#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Relations/fullProgs.rkt"
         "../../../Relations/terms.rkt"
         "../../../Relations/termsValStore.rkt"
         "../../../Relations/termsObjStore.rkt"
         "../../../Relations/termsValObjStore.rkt"
         "../../../Relations/meta.rkt"
         "./prepare.rkt")

(define (check_one_step c result)
  (or
   ; it was a final configuration 
   (and (= (length result) 0)
        (term (is_final_conf ,c)))
   ; not a final configuration 
   (and (= (length result) 1)
        (term (well_formed_conf ,(first result))))))

(define (soundness_wfc_pred c debug)
  (let ([result (if (not (term (well_formed_conf ,c)))
                    ; TODO: naive approach to discard ill formed
                    ; terms
                    (if debug
                        (begin (println (term ,c))
                               (term ((() : () : \;))))
                        
                        (term ((() : () : \;))))
                     
                    (apply-reduction-relation full-progs-rel
                                              (term ,c)))])
    (check_one_step c result)
    )
  )

; attempt-size parameter of redex-check
(define (attempt-size n)
  (inexact->exact (floor (log (max n 1) 4))))

; generates "attempts" examples taking into account left hand-side of the rules
; from "rel"; flag "debug" indicates if the non-well-formed terms must be printed
(define (soundness_wfc rel attempts debug)
  (redex-check  ext-lang any
                (soundness_wfc_pred (term any) debug)
                #:prepare close_conf
                #:attempts attempts
                #:source rel
                #:attempt-size attempt-size))

; test and print relation coverage of soundness_wfc
(define (soundness_wfc_coverage rel attempts debug)
  ; create records to register test coverage related with ↦
  (let ([rel-coverage (make-coverage rel)]
        [full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list rel-coverage full-progs-rel-coverage)])
      (soundness_wfc rel attempts debug)
      (values (covered-cases rel-coverage)
              (covered-cases full-progs-rel-coverage))
      )))

; generates "attempts" examples following the pattern (σ : θ : s)
; flag "debug" indicates if the non-well-formed terms must be printed
(define (soundness_wfc_no_rel attempts debug)
  (redex-check  ext-lang
                (σ
                 : (((objr 1) ((\{ (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 2) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 3) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 4) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 5) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   \}) nil pos))
                    osp ...)
                 : wfs)
                (soundness_wfc_pred (term (σ
                                           :
                                           (((objr 1) ((\{ (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 2) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 3) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 4) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   (\[ "__call" \] = v_15)
                                   \}) nil pos))
                    ((objr 5) ((\{ (\[ "__add" \] = v_1)
                                   (\[ "__sub" \] = v_2)
                                   (\[ "__mul" \] = v_3)
                                   (\[ "__div" \] = v_4)
                                   (\[ "__mod" \] = v_5)
                                   (\[ "__pow" \] = v_6)
                                   (\[ "__unm" \] = v_7)
                                   (\[ "__concat" \] = v_8)
                                   (\[ "__len" \] = v_9)
                                   (\[ "__eq" \] = v_10)
                                   (\[ "__lt" \] = v_11)
                                   (\[ "__le" \] = v_12)
                                   (\[ "__index" \] = v_13)
                                   (\[ "__newindex" \] = v_14)
                                   \}) nil pos))
                    osp ...) : wfs)) debug)
                #:prepare close_conf
                #:attempts attempts
                #:attempt-size attempt-size))

; tests and prints relation coverage of soundness_wfc_no_rel
(define (soundness_wfc_no_rel_coverage attempts debug)
  ; create records to register test coverage related with ↦
  (let ([full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list full-progs-rel-coverage)])
      (soundness_wfc_no_rel attempts debug)
      (values (covered-cases full-progs-rel-coverage))
      )))

; generates "attempts" examples following the pattern
; (σ : θ : (return ($builtIn builtinserv (v ...))))
; flag "debug" indicates if the non-well-formed terms must be printed
(define (soundness_wfc_builtIn attempts debug)
  (redex-check  ext-lang (σ : θ : (return ($builtIn builtinserv (v ...))))
                (soundness_wfc_pred (term (σ : θ : (return ($builtIn builtinserv (v ...))))) debug)
                #:prepare close_conf
                #:attempts attempts
                #:attempt-size attempt-size
                ))

; test and print relation coverage soundness_wfc_builtIn
(define (soundness_wfc_builtIn_coverage attempts debug)
  ; create records to register test coverage related with ↦
  (let ([full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list full-progs-rel-coverage)])
      (soundness_wfc_builtIn attempts debug)
      (values (covered-cases full-progs-rel-coverage)))))


; TODO: any way to automate this?
(define terms-rel-rules 50)
(define terms-val-store-rules 3)
(define terms-obj-store-rules 11)
(define terms-val-obj-store-rules 3)
(define meta-rules 23)
(define full-rules (+ terms-rel-rules
                      terms-val-store-rules
                      terms-obj-store-rules
                      terms-val-obj-store-rules
                      meta-rules))
; ratio of the examples generated following the left side of rules, with respect
; to the total amount of examples
(define ratio-prepare (/ 2 3))

; divides tests among every relation, according to ratio-prepare;
; tests soundness_wfc for every relation, except for full-progs-rel;
; invokes soundness_wfc_no_rel for full-progs-rel
(define (soundness_wfc_full_coverage attempts debug)
  ; create records to register test coverage related with ↦
  (let ([terms-rel-coverage (make-coverage terms-rel)]
        [terms-val-store-coverage (make-coverage terms-val-store)]
        [terms-obj-store-coverage (make-coverage terms-obj-store)]
        [terms-val-obj-store-coverage (make-coverage terms-val-obj-store)]
        [meta-coverage (make-coverage meta)]
        [full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list terms-rel-coverage
                                  terms-val-store-coverage
                                  terms-obj-store-coverage
                                  terms-val-obj-store-coverage
                                  meta-coverage
                                  full-progs-rel-coverage)])
      (begin
        ; 50 rules in terms-rel
        (soundness_wfc terms-rel (floor (* (* attempts (/ terms-rel-rules
                                                          full-rules))
                                           ratio-prepare)) debug)
        ; 3 rules in terms-val-store
        (soundness_wfc terms-val-store (floor (* (* attempts (/ terms-val-store-rules
                                                                full-rules))
                                                 ratio-prepare)) debug)
        ; 11 rules in terms-obj-store
        (soundness_wfc terms-obj-store (floor (* (* attempts (/ terms-obj-store-rules
                                                                full-rules))
                                                 ratio-prepare)) debug)
        ; 3 rules in terms-val-obj-store
        (soundness_wfc terms-val-obj-store (floor (* (* attempts (/ terms-val-obj-store-rules
                                                                    full-rules))
                                                     ratio-prepare)) debug)
        ; 23 rules in meta
        (soundness_wfc meta (floor (* (* attempts (/ meta-rules
                                                     full-rules))
                                      ratio-prepare)) debug)

        ; remaining tests taken just from arbitrary terms of the grammar
        (soundness_wfc_no_rel (floor (* attempts
                                        (- 1 ratio-prepare))) debug)
        
        (values (covered-cases terms-rel-coverage)
                (covered-cases terms-val-store-coverage)
                (covered-cases terms-obj-store-coverage)
                (covered-cases terms-val-obj-store-coverage)
                (covered-cases meta-coverage)
                (covered-cases full-progs-rel-coverage))
        ))))