#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (test-closure)
  (test-suite "closure.lua"
              (list "assert"
                    "collectgarbage"
                    "error"
                    "pcall"
                    "print"
                    "math"
                    "math.sin"
                    "setmetatable"
                    "getmetatable")))

(provide test-closure)