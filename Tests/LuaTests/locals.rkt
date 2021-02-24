#lang racket
(require "./tests_aux.rkt")

; local variables and environments
(define (test-locals_1)
  (test-suite "locals_1.lua"
              (list "assert"
                    "load"
                    "print"
                    "select"
                    "type")))

(define (test-locals_2)
  (test-suite "locals_2.lua"
              (list "_G"
                    "assert"
                    "load"
                    "print")))

(provide test-locals_1 test-locals_2)