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
;
; NOTE: if the code being executed makes use of the service "load", and the
; library services used in the code passed to load are not stated explicitly,
; it will be required to provide by hand every library service used by the
; snippet being executed; for example:
; (execute "(load(\"return ty\" .. \"pe(1)\"))()")
; ...
; ($err "attempt to call a nil value.")
;
; in this case we will need to reduce the previous term by invoking:
; 
; (apply-reduction-relation* full-progs-rel
;        (plugIntoExecutionEnvironment services
;                                      '("load" "type")
;                                      (parse-this "(load(\"return ty\" .. \"pe(1)\"))()"
;                                                  #f (void))))

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