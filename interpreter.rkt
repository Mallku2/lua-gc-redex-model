#lang racket
(require redex
         "./grammar.rkt"
         "./executionEnvironment.rkt"
         "Desugar/parser.rkt"
         "Relations/fullProgs.rkt"
         "Relations/gc.rkt"
         )

; PARAM : code, the actual Lua program to be executed, as a string value
; PARAM : selec_servs, a list containing the name of the library services
; required for the execution of the program
;
; example:
; (execute "print(\"hello, world!\")" (list "print"))

(define (execute code selec_servs)
  (apply-reduction-relation* full-progs-rel
        (plugIntoExecutionEnvironment services
                                      selec_servs
                                      (parse-this code #f (void)))))

(define (luatrace code selec_servs)
  (begin
    (reduction-steps-cutoff 500)
    (traces full-progs-rel
        (plugIntoExecutionEnvironment services
                                      selec_servs
                                      (parse-this code #f (void))))))

(define (luagctrace code selec_servs)
  (begin
    (reduction-steps-cutoff 500)
    (traces gc-rel
        (plugIntoExecutionEnvironment services
                                      selec_servs
                                      (parse-this code #f (void))))))

(provide execute
         luatrace
         luagctrace)