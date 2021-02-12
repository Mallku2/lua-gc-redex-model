#lang racket
(require "./tests_aux.rkt")

; semantics of set and get metatable
; metamethod for tostring
; protected metatables
; metamethod for new index
(define (test-events_1)
  (test-suite "events_1.lua"
              (list "assert"
                    "collectgarbage"
                    "getmetatable"
                    "pcall"
                    "print"
                    "rawset"
                    "setmetatable"
                    "tostring")))

; metamethod for index and new index
; metamethods for arith ops
(define (test-events_2)
  (test-suite "events_2.lua"
              (list "assert"
                    "collectgarbage"
                    "getmetatable"
                    "rawget"
                    "rawset"
                    "setmetatable"
                    "table"
                    "table.unpack")))

; metamethod for #, <, <=, ==, ..
(define (test-events_3a)
  (test-suite "events_3a.lua"
              (list "assert"
                    "collectgarbage"
                    "pcall"
                    "rawlen"
                    "setmetatable"
                    "type")))

(define (test-events_3b)
  (test-suite "events_3b.lua"
              (list "assert"
                    "collectgarbage"
                    "setmetatable"
                    "type")))

(define (test-events_3c)
  (test-suite "events_3c.lua"
              (list "assert"
                    "collectgarbage"
                    "getmetatable"
                    "next"
                    "pairs"
                    "rawequal"
                    "setmetatable"
                    "type")))

(define (test-events_4)
  (test-suite "events_4.lua"
              (list "_G"
                    "assert"
                    "debug"
                    "debug.setmetatable"
                    "getmetatable"
                    "pcall"
                    "print"
                    "rawget"
                    "rawset"
                    "setmetatable"
                    "type")))

(provide test-events_1 test-events_2
         test-events_3a test-events_3b test-events_3c
         test-events_4)