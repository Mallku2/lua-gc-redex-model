#lang racket
(require "./tests_aux.rkt")

; testing (parts of) table library
; table.unpack
; table.pack
(define (test-sort)
  (test-suite "sort.lua"))
(provide test-sort)