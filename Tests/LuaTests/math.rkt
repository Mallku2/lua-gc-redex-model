#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

; basic float notation
; numeric strings
(define (test-math_1)
  (test-suite "math_1.lua"))

; tonumber
; 'tonumber' with base
; 'tonumber' fo invalid formats
; 'tonumber' for invalid hexadecimal formats
(define (test-math_2)
  (test-suite "math_2.lua"))

; hexadecimal numerals
; floating hexas
; order operators
(define (test-math_3)
  (test-suite "math_3.lua"))

; mod operator
; trigonometric functions
; constant limits
(define (test-math_4)
  (test-suite "math_4.lua"))

; implicit convertions
(define (test-math_5)
  (test-suite "math_5.lua"))

(provide test-math_1 test-math_2 test-math_3 test-math_4 test-math_5)