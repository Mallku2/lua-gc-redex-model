#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (test-nextvar_1)
  (test-suite "nextvar_1.lua"
              (list "print"
                    "assert"
                    "ipairs"
                    "type"
                    )))

(define (test-nextvar_2)
  (test-suite "nextvar_2.lua"
              (list "_G"
                    "print"
                    "next"
                    "assert"
                    "pairs"
                    "math"
                    "math.fmod"
                    )))

(define (test-nextvar_3)
  (test-suite "nextvar_3.lua"
              (list "_G"
                    "print"
                    "next"
                    "assert"
                    "pairs"
                    )))

(define (test-nextvar_4)
  (test-suite "nextvar_4.lua"
              (list "_G"
                    "print"
                    "next"
                    "assert"
                    "pairs"
                    "math"
                    "math.max"
                    "math.pi"
                    "string"
                    "string.rep"
                    "collectgarbage"
                    "table"
                    "pcall"
                    "type"
                    "table"
                    "table.insert"
                    )))

(define (test-nextvar_5)
  (test-suite "nextvar_5.lua"
              (list "next"
                    "assert"
                    "error"
                    "pairs"
                    "print"
                    "collectgarbage"
                    "table"
                    "table.unpack"
                    )))

(define (test-nextvar_6)
  (test-suite "nextvar_6.lua"
              (list "assert"
                    "type"
                    "setmetatable"
                    "ipairs"
                    "pairs"
                    "print"
                    )))