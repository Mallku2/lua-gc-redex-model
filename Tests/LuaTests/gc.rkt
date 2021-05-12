#lang racket
(require "./tests_aux.rkt")

; testing garbage collection
; functions with errors
(define (test-gc_1)
  (test-suite "gc_1.lua"))

; long strings
; clearing tables
(define (test-gc_2)
  (test-suite "gc_2.lua"))

; weak tables
(define (test-gc_3a)
  (test-suite "gc_3a.lua"))

(define (test-gc_3b)
  (test-suite "gc_3b.lua"))

(define (test-gc_3c)
  (test-suite "gc_3c.lua"))

(define (test-gc_3d)
  (test-suite "gc_3d.lua"))

(define (test-gc_3e)
  (test-suite "gc_3e.lua"))

(define (test-gc_3f)
  (test-suite "gc_3f.lua"))

; testing errors during GC
(define (test-gc_4)
  (test-suite "gc_4.lua"))

(provide test-gc_1 test-gc_2 test-gc_3a test-gc_3b test-gc_3c
         test-gc_3d test-gc_3e test-gc_3f test-gc_4)