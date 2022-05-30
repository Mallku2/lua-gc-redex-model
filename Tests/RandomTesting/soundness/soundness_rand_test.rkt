#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Relations/fullProgs.rkt"
         "../../../Relations/terms.rkt"
         "../../../Relations/termsValStore.rkt"
         "../../../Relations/termsObjStore.rkt"
         "../../../Relations/termsValObjStore.rkt"
         "../../../Relations/meta.rkt"
         "./prepare.rkt"
         "./wfc.rkt")

; auxiliar function that checks progress, preservation of well-formedness
; and determinism of the semantics
(define (check_one_step c result)
  (or
   ; it was a final configuration 
   (and (= (length result) 0)
        (term (is_final_conf ,c)))
   ; not a final configuration 
   (and (= (length result) 1)
        (term (well_formed_conf ,(first result))))))

; checks soundness of wfc and determinism of the semantics
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
; the default value of attempt-size computes a log5 bound, we cannot
; increase it too much without having a notable impact on performance
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
                 : (((objr 1) ((\{ (\[ "__concat" \] = v_1)
                                   (\[ "__len" \] = v_2)
                                   (\[ "__eq" \] = v_3)
                                   (\[ "__lt" \] = v_4)
                                   (\[ "__le" \] = v_5)
                                   (\[ "__index" \] = v_6)
                                   (\[ "__newindex" \] = v_7)
                                   (\[ "__call" \] = v_8)
                                   \}) nil pos_1))
                    ((objr 2) ((\{ (\[ "__add" \] = v_9)
                                   (\[ "__sub" \] = v_10)
                                   (\[ "__mul" \] = v_11)
                                   (\[ "__div" \] = v_12)
                                   (\[ "__mod" \] = v_13)
                                   (\[ "__pow" \] = v_14)
                                   (\[ "__unm" \] = v_15)
                                   (\[ "__concat" \] = v_16)
                                   (\[ "__len" \] = v_17)
                                   (\[ "__eq" \] = v_18)
                                   (\[ "__lt" \] = v_19)
                                   (\[ "__le" \] = v_20)
                                   (\[ "__index" \] = v_21)
                                   (\[ "__newindex" \] = v_22)
                                   (\[ "__call" \] = v_23)
                                   \}) nil pos_2))
                    ((objr 3) ((\{ (\[ "__add" \] = v_24)
                                   (\[ "__sub" \] = v_25)
                                   (\[ "__mul" \] = v_26)
                                   (\[ "__div" \] = v_27)
                                   (\[ "__mod" \] = v_28)
                                   (\[ "__pow" \] = v_29)
                                   (\[ "__unm" \] = v_30)
                                   (\[ "__concat" \] = v_31)
                                   (\[ "__len" \] = v_32)
                                   (\[ "__eq" \] = v_33)
                                   (\[ "__lt" \] = v_34)
                                   (\[ "__le" \] = v_35)
                                   (\[ "__index" \] = v_36)
                                   (\[ "__newindex" \] = v_37)
                                   (\[ "__call" \] = v_38)
                                   \}) nil pos_3))
                    ((objr 4) ((\{ (\[ "__add" \] = v_39)
                                   (\[ "__sub" \] = v_40)
                                   (\[ "__mul" \] = v_41)
                                   (\[ "__div" \] = v_42)
                                   (\[ "__mod" \] = v_43)
                                   (\[ "__pow" \] = v_44)
                                   (\[ "__unm" \] = v_45)
                                   (\[ "__eq" \] = v_46)
                                   (\[ "__lt" \] = v_47)
                                   (\[ "__le" \] = v_48)
                                   (\[ "__index" \] = v_49)
                                   (\[ "__newindex" \] = v_50)
                                   (\[ "__call" \] = v_51)
                                   \}) nil pos_4))
                    ((objr 5) ((\{ (\[ "__add" \] = v_52)
                                   (\[ "__sub" \] = v_53)
                                   (\[ "__mul" \] = v_54)
                                   (\[ "__div" \] = v_55)
                                   (\[ "__mod" \] = v_56)
                                   (\[ "__pow" \] = v_57)
                                   (\[ "__unm" \] = v_58)
                                   (\[ "__concat" \] = v_59)
                                   (\[ "__len" \] = v_60)
                                   (\[ "__eq" \] = v_61)
                                   (\[ "__lt" \] = v_62)
                                   (\[ "__le" \] = v_63)
                                   (\[ "__index" \] = v_64)
                                   (\[ "__newindex" \] = v_65)
                                   \}) nil pos_5))
                    osp ...)
                 : wfs)
                (soundness_wfc_pred (term (σ
                 : (((objr 1) ((\{ (\[ "__concat" \] = v_1)
                                   (\[ "__len" \] = v_2)
                                   (\[ "__eq" \] = v_3)
                                   (\[ "__lt" \] = v_4)
                                   (\[ "__le" \] = v_5)
                                   (\[ "__index" \] = v_6)
                                   (\[ "__newindex" \] = v_7)
                                   (\[ "__call" \] = v_8)
                                   \}) nil pos_1))
                    ((objr 2) ((\{ (\[ "__add" \] = v_9)
                                   (\[ "__sub" \] = v_10)
                                   (\[ "__mul" \] = v_11)
                                   (\[ "__div" \] = v_12)
                                   (\[ "__mod" \] = v_13)
                                   (\[ "__pow" \] = v_14)
                                   (\[ "__unm" \] = v_15)
                                   (\[ "__concat" \] = v_16)
                                   (\[ "__len" \] = v_17)
                                   (\[ "__eq" \] = v_18)
                                   (\[ "__lt" \] = v_19)
                                   (\[ "__le" \] = v_20)
                                   (\[ "__index" \] = v_21)
                                   (\[ "__newindex" \] = v_22)
                                   (\[ "__call" \] = v_23)
                                   \}) nil pos_2))
                    ((objr 3) ((\{ (\[ "__add" \] = v_24)
                                   (\[ "__sub" \] = v_25)
                                   (\[ "__mul" \] = v_26)
                                   (\[ "__div" \] = v_27)
                                   (\[ "__mod" \] = v_28)
                                   (\[ "__pow" \] = v_29)
                                   (\[ "__unm" \] = v_30)
                                   (\[ "__concat" \] = v_31)
                                   (\[ "__len" \] = v_32)
                                   (\[ "__eq" \] = v_33)
                                   (\[ "__lt" \] = v_34)
                                   (\[ "__le" \] = v_35)
                                   (\[ "__index" \] = v_36)
                                   (\[ "__newindex" \] = v_37)
                                   (\[ "__call" \] = v_38)
                                   \}) nil pos_3))
                    ((objr 4) ((\{ (\[ "__add" \] = v_39)
                                   (\[ "__sub" \] = v_40)
                                   (\[ "__mul" \] = v_41)
                                   (\[ "__div" \] = v_42)
                                   (\[ "__mod" \] = v_43)
                                   (\[ "__pow" \] = v_44)
                                   (\[ "__unm" \] = v_45)
                                   (\[ "__eq" \] = v_46)
                                   (\[ "__lt" \] = v_47)
                                   (\[ "__le" \] = v_48)
                                   (\[ "__index" \] = v_49)
                                   (\[ "__newindex" \] = v_50)
                                   (\[ "__call" \] = v_51)
                                   \}) nil pos_4))
                    ((objr 5) ((\{ (\[ "__add" \] = v_52)
                                   (\[ "__sub" \] = v_53)
                                   (\[ "__mul" \] = v_54)
                                   (\[ "__div" \] = v_55)
                                   (\[ "__mod" \] = v_56)
                                   (\[ "__pow" \] = v_57)
                                   (\[ "__unm" \] = v_58)
                                   (\[ "__concat" \] = v_59)
                                   (\[ "__len" \] = v_60)
                                   (\[ "__eq" \] = v_61)
                                   (\[ "__lt" \] = v_62)
                                   (\[ "__le" \] = v_63)
                                   (\[ "__index" \] = v_64)
                                   (\[ "__newindex" \] = v_65)
                                   \}) nil pos_5))
                    osp ...)
                 : wfs)) debug)
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