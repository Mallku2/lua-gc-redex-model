#lang racket
(require "./tests_aux.rkt")

; local variables and environments
(define (test-locals_1)
  (test-suite "locals_1.lua"))

(define (test-locals_2)
  (test-suite "locals_2.lua"))

(provide test-locals_1 test-locals_2)