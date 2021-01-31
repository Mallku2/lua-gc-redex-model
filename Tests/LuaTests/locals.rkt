#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (locals)
  (test-suite "locals.lua"
              (list "assert"
                    "collectgarbage"
                    "load"
                    "print"
                    "type")))
