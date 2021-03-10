#lang racket
(require "./tests_aux.rkt")

; testing syntax
; testing semicollons
; testing priorities
(define (test-constructs_1)
  (test-suite "constructs_1.lua"))

; testing loops (in three files)
(define (test-constructs_2a)
  (test-suite "constructs_2a.lua"))

(define (test-constructs_2b)
  (test-suite "constructs_2b.lua"))

(define (test-constructs_2c)
  (test-suite "constructs_2c.lua"))

(define (test-constructs_2d)
  (test-suite "constructs_2d.lua"))

; short-circuit optimizations
(define (test-constructs_3)
  (test-suite "constructs_3.lua"))

(provide test-constructs_1 test-constructs_2a test-constructs_2b
         test-constructs_2c test-constructs_2d test-constructs_3)