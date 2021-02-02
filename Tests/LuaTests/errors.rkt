#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (errors)
  (test-suite "errors.lua"
              (list "assert"
                    "collectgarbage"
                    "getmetatable"
                    "pcall"
                    "xpcall"
                    "print"
                    "load"
                    "table"
                    "table.unpack"
                    "tonumber"
                    "tostring"
                    "error"
                    "type"
                    "math"
                    "math.sin"
                    "string")))

