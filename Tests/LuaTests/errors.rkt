#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (test-errors)
  (test-suite "errors.lua"
              (list "print"
                    "getmetatable"
                    "load"
                    "pcall"
                    "assert"
                    "error"
                    "table"
                    "table.unpack"
                    "math"
                    "math.sin"
                    "tostring"
                    "tonumber"
                    "next"
                    "collectgarbage"
                    "table.concat"
                    "math.cos"
                    "setmetatable"
                    "string"
                    "string.sub"
                    "xpcall"
                    )))

(provide test-errors)