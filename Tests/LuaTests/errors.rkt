#lang racket
(require "./tests_aux.rkt")

; error message with no extra info
; common errors/errors that crashed in the past
; tests for better error messages
(define (test-errors_1a)
  (test-suite "errors_1a.lua"))

(define (test-errors_1b)
  (test-suite "errors_1b.lua"))

; global functions
(define (test-errors_2)
  (test-suite "errors_2.lua"))

; error in error handling
(define (test-errors_3)
  (test-suite "errors_3.lua"))

(provide test-errors_1a test-errors_1b test-errors_2 test-errors_3)