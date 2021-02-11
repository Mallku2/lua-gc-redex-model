#lang racket
(require redex
         "./tests_aux.rkt")

; 'type'
; local-function recursion
; declarations
(define (test-calls_1a)
  (test-suite "calls_1a.lua"
              (list "assert"
                    "print"
                    "type")))

(define (test-calls_1b)
  (test-suite "calls_1b.lua"
              (list "assert"
                    "print")))

(define (test-calls_1c)
  (test-suite "calls_1c.lua"
              (list "assert"
                    "load"
                    "print")))

; closures
; multiple returns
; calls with 'incorrect' arguments
(define (test-calls_2a)
  (test-suite "calls_2a.lua"
              (list "assert"
                    "collectgarbage"
                    "print")))

(define (test-calls_2b)
  (test-suite "calls_2b.lua"
              (list "assert"
                    "collectgarbage"
                    "math"
                    "math.sin"
                    "rawget"
                    "rawset"
                    "table"
                    "table.pack")))

; generic load
(define (test-calls_3a)
  (test-suite "calls_3a.lua"
              (list "_G"
                    "assert"
                    "collectgarbage"
                    "load"
                    "string"
                    "string.sub")))

(define (test-calls_3b)
  (test-suite "calls_3b.lua"
              (list "_G"
                    "assert"
                    "load"
                    "string"
                    "string.dump"
                    "type")))

(define (test-calls_3c)
  (test-suite "calls_3c.lua"
              (list "_G"
                    "assert"
                    "collectgarbage"
                    "load"
                    "string"
                    "string.dump"
                    "string.sub")))

; load when _ENV is not first upvalue
(define (test-calls_4)
  (test-suite "calls_4.lua"
              (list "_G"
                    "assert"
                    "load"
                    "string"
                    "string.dump")))

; generic load with nested functions
(define (test-calls_5)
  (test-suite "calls_5.lua"
              (list "assert"
                    "collectgarbage"
                    "load"
                    "string"
                    "string.sub")))

; bug in parameter adjustment
(define (test-calls_6)
  (test-suite "calls_6.lua"
              (list "assert"
                    "print")))

(provide test-calls_1a test-calls_1b test-calls_1c
         test-calls_2a test-calls_2b
         test-calls_3a test-calls_3b test-calls_3c
         test-calls_4 test-calls_5 test-calls_6)