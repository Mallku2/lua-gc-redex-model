#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

; testing 'type'
; testing local-function recursion
; testing declarations
(define (test-calls_1)
  (test-suite "calls_1.lua"
              (list "assert"
                    "collectgarbage"
                    "load"
                    "print"
                    "type")))

; testing closures
; testing multiple returns
; testing calls with 'incorrect' arguments
(define (test-calls_2)
  (test-suite "calls_2.lua"
              (list "assert"
                    "collectgarbage"
                    "math"
                    "math.sin"
                    "print"
                    "rawget"
                    "rawset"
                    "table"
                    "table.pack")))

; test for generic load
; load when _ENV is not first upvalue
; test generic load with nested functions
; test for bug in parameter adjustment
; NOTE: this module ends by returning a variable (deep) defined in
; another module; hence, the laste configurations ends with
; return nil
(define (test-calls_3)
  (test-suite "calls_3.lua"
              (list "_G"
                    "assert"
                    "collectgarbage"
                    "load"
                    "print"
                    "string"
                    "string.dump"
                    "string.sub"
                    "type")))

(provide test-calls_1 test-calls_2 test-calls_3)