#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (test-sort)
  (test-suite "sort.lua"
              (list "print"
                    "assert"
                    "table"
                    "table.unpack"
                    "table.pack"
                    "next"
                    "select"
                    "pcall"
                    )))
