#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (test-math_1)
  (test-suite "math_1.lua"
              (list "assert"
                    "print"
                    "type"
                    "math"
                    "math.modf"
                    "math.huge")))

(define (test-math_2)
  (test-suite "math_2.lua"
              (list "assert"
                    "print"
                    "tonumber"
                    "string"
                    "string.rep"
                    "select"
                    )))

(define (test-math_3)
  (test-suite "math_3.lua"
              (list "assert"
                    "print"
                    "tonumber"
                    "math"
                    "math.abs"
                    )))

(define (math-all)
  (filter (lambda (x) (string-contains? x "math")) (dict-keys services)))

(define (test-math_4)
  (test-suite "math_4.lua"
              (append (list "assert"
                    "print"
                    "tonumber") (math-all))))

(define (test-math_5)
  (test-suite "math_5.lua"
              (list "assert"
                    "print"
                    "tonumber"
                    "math"
                    "math.huge"
                    "pcall")))
