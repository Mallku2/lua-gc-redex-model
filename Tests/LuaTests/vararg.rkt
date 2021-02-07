#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (test-vararg_1)
  (test-suite "vararg_1.lua"
              (list "print"
                    "assert"
                    "select"
                    "type"
                    "table"
                    "table.unpack"
                    "math"
                    "math.max"
                    "load"
                    "pcall"
                    "next"
                    )))

(define (test-vararg_2)
  (test-suite "vararg_2.lua"
              (list "print"
                    "assert"
                    "select"
                    "type"
                    "table"
                    "table.unpack"
                    "math"
                    "math.max"
                    "load"
                    "pcall"
                    "next"
                    )))

(provide test-vararg_1 test-vararg_2)