#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

;; testing syntax
;; testing semicollons
;; testing priorities
(define (constructs_1)
  (test-suite "constructs_1.lua"
              (list "print"
                    "assert"
                    "tonumber")))

;; testing loops (in three files)
(define (constructs_2a)
  (test-suite "constructs_2a.lua"
              (list "assert"
                    "collectgarbage"
                    "print"
                    "type"
                    "math"
                    "math.sin")))

(define (constructs_2b)
  (test-suite "constructs_2b.lua"
              (list "assert"
                    "collectgarbage"
                    "print"
                    "type"
                    "math"
                    "math.sin")))

(define (constructs_2c)
  (test-suite "constructs_2c.lua"
              (list "assert"
                    "collectgarbage"
                    "print"
                    "type"
                    "math"
                    "math.sin")))

(define (constructs_2d)
  (test-suite "constructs_2d.lua"
              (list "assert"
                    "collectgarbage"
                    "print"
                    "type"
                    "math"
                    "math.sin")))

;; misc
(define (constructs_3)
  (test-suite "constructs_3.lua"
              (list "assert"
                    "load"
                    "ipairs"
                    "pairs"
                    "collectgarbage"
                    "print")))

(define (constructs)
  (test-suite "constructs.lua"
              (list "assert"
                    "collectgarbage"
                    "print"
                    "tonumber"
                    "type"
                    "load"
                    "ipairs"
                    "pairs"
                    "math"
                    "math.sin")))

