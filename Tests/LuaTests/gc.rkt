#lang racket
(require "./tests_aux.rkt")

; testing garbage collection
; functions with errors
(define (test-gc_1)
  (test-suite "gc_1.lua"
              (list "assert"
                    "collectgarbage"
                    "load"
                    "pcall"
                    "print"
                    "string"
                    "string.len"
                    "string.sub"
                    )))

; long strings
; clearing tables
(define (test-gc_2)
  (test-suite "gc_2.lua"
              (list "assert"
                    "collectgarbage"
                    "error"
                    "next"
                    "pairs"
                    "print"
                    "string"
                    "string.len"
                    "tostring"
                    "type"
                    )))

; weak tables
(define (test-gc_3a)
  (test-suite "gc_3a.lua"
              (list "assert"
                    "collectgarbage"
                    "pairs"
                    "print"
                    "setmetatable"
                    "string"
                    "string.rep"
                    )))

(define (test-gc_3b)
  (test-suite "gc_3b.lua"
              (list "assert"
                    "collectgarbage"
                    "pairs"
                    "setmetatable"
                    "string"
                    "string.rep"
                    )))

(define (test-gc_3c)
  (test-suite "gc_3c.lua"
              (list "assert"
                    "collectgarbage"
                    "next"
                    "pairs"
                    "setmetatable"
                    "string"
                    "string.rep"
                    )))

(define (test-gc_3d)
  (test-suite "gc_3d.lua"
              (list "assert"
                    "collectgarbage"
                    "next"
                    "setmetatable"
                    "type"
                    )))