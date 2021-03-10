#lang racket
(require "./tests_aux.rkt")

; vararg functions
(define (test-vararg_1)
  (test-suite "vararg_1.lua"))

(define (test-vararg_2)
  (test-suite "vararg_2.lua"))

(define (test-vararg_3)
  (test-suite "vararg_3.lua"))

; new-style varargs
(define (test-vararg_4)
  (test-suite "vararg_4.lua"))

; varargs for main chunks
(define (test-vararg_5)
  (test-suite "vararg_5.lua"))

(provide test-vararg_1 test-vararg_2 test-vararg_3 test-vararg_4 test-vararg_5)