#lang racket
(require "./tests_aux.rkt")

; tables, next, and for
; ipairs
(define (test-nextvar_1)
  (test-suite "nextvar_1.lua"))

; size operation on empty tables
; next uses always the same iteraction function
(define (test-nextvar_2)
  (test-suite "nextvar_2.lua"))

; next
(define (test-nextvar_3)
  (test-suite "nextvar_3.lua"))

; pairs
; erasing values in tables during iteration
; table.insert
(define (test-nextvar_4)
  (test-suite "nextvar_4.lua"))

; next
; testing precision in numeric for
(define (test-nextvar_5)
  (test-suite "nextvar_5.lua"))

; __pairs and __ipairs metamethod
(define (test-nextvar_6)
  (test-suite "nextvar_6.lua"))

(provide test-nextvar_1 test-nextvar_2 test-nextvar_3 test-nextvar_4 test-nextvar_5 test-nextvar_6)