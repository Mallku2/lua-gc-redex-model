#lang racket
(require redex
         "./grammar.rkt"
         "./executionEnvironment.rkt"
         "Desugar/parser.rkt"
         "Relations/fullProgs.rkt"
         "Relations/gc.rkt"
         "Tests/LuaTests/tests_aux.rkt")

; PARAM : code, the actual Lua program to be executed, as a string value
; required for the execution of the program
;
; example:
; (execute "print(\"hello, world!\")")

(define (execute code)
  (apply-reduction-relation* full-progs-rel
        (plugIntoExecutionEnvironment services
                                      (services-from code)
                                      (parse-this code #f (void)))))

(define (luatrace code)
  (begin
    (reduction-steps-cutoff 500)
    (traces full-progs-rel
        (plugIntoExecutionEnvironment services
                                      (services-from code)
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