#lang racket
(require redex
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (string-all)
  (services-from "string"))

(define (test-strings_1)
  (test-suite "strings_1.lua"
              (append
               (list "assert"
                    "collectgarbage"
                    "pcall"
                    "print"
                    "tostring"
                    "setmetatable"
                    "type"
                    "table"
                    "table.concat"
                    )
              (string-all))))

(define (test-strings_2)
  (test-suite "strings_2.lua"
              (append
               (list "assert"
                    "collectgarbage"
                    "pcall"
                    "print"
                    "tostring"
                    "setmetatable"
                    "type"
                    "table"
                    "table.concat"
                    )
              (string-all))))

(define (test-strings_3)
  (test-suite "strings_3.lua"
              (append
               (list "assert"
                    "collectgarbage"
                    "pcall"
                    "print"
                    "tostring"
                    "setmetatable"
                    "type"
                    "table"
                    "table.concat"
                    )
              (string-all))))

(provide test-strings_1 test-strings_2 test-strings_3)