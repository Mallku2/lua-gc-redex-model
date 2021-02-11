#lang racket
(require "./tests_aux.rkt")

; gc
; equality
(define (test-closure_1)
  (test-suite "closure_1.lua"
              (list "assert"
                    "collectgarbage"
                    "getmetatable"
                    "math"
                    "math.sin"
                    "print"
                    "setmetatable")))

; closures with 'for' control variable
; closures with 'for' control variable x break
; closure x break x return x errors
(define (test-closure_2)
  (test-suite "closure_2.lua"
              (list "assert"
                    "error"
                    "pcall"
                    "print")))

; multi-level closure
; closures x repeat-until
; correctly closing upvalues in tail calls of vararg functions
(define (test-closure_3)
  (test-suite "closure_3.lua"
              (list "assert"
                    "print")))

(provide test-closure_1 test-closure_2 test-closure_3)