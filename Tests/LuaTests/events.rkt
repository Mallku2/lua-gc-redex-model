#lang racket
(require "./tests_aux.rkt")

; semantics of set and get metatable
; metamethod for tostring
; protected metatables
; metamethod for new index
(define (test-events_1)
  (test-suite "events_1.lua"))

; metamethod for index and new index
; metamethods for arith ops
(define (test-events_2)
  (test-suite "events_2.lua"))

; metamethod for #, <, <=, ==, ..
(define (test-events_3a)
  (test-suite "events_3a.lua"))

(define (test-events_3b)
  (test-suite "events_3b.lua"))

(define (test-events_3c)
  (test-suite "events_3c.lua"))

(define (test-events_4)
  (test-suite "events_4.lua"))

(provide test-events_1 test-events_2
         test-events_3a test-events_3b test-events_3c
         test-events_4)