#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (test-locals_1)
  (test-suite "locals_1.lua"
              (list "assert"
                    "select"
                    "load"
                    "collectgarbage"
                    "load"
                    "print"
                    "type")))

(define (test-locals_2)
  (test-suite "locals.lua"
              (list "_G"
                    "assert"
                    "select"
                    "load"
                    "collectgarbage"
                    "load"
                    "print"
                    "type")))
