#lang racket
(require "./tests_aux.rkt")

; error message with no extra info
; common errors/errors that crashed in the past
; tests for better error messages
(define (test-errors_1a)
  (test-suite "errors_1a.lua"
              (list "_G"
                    "print"
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
                    "tonumber")))

(define (test-errors_1b)
  (test-suite "errors_1b.lua"
              (list "assert"
                    "load"
                    "math"
                    "math.sin"
                    "next"
                    "pcall"
                    "print")))

; global functions
(define (test-errors_2)
  (test-suite "errors_2.lua"
              (list "assert"
                    "collectgarbage"
                    "load"
                    "math"
                    "math.sin"
                    "math.cos"
                    "pcall"
                    "print"
                    "setmetatable"
                    "string"
                    "string.sub"
                    "table"
                    "table.concat")))

; error in error handling
(define (test-errors_3)
  (test-suite "errors_3.lua"
              (list "assert"
                    "error"
                    "load"
                    "pcall"
                    "print"
                    "type"
                    "xpcall")))

(provide test-errors_1a test-errors_1b test-errors_2 test-errors_3)