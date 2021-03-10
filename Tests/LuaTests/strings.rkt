#lang racket
(require "./tests_aux.rkt")

; strings and string library

; order
; string.sub
(define (test-strings_1)
  (test-suite "strings_1.lua"))

; string.len
; #
; string.rep
; string.reverse
; tostring
(define (test-strings_2)
  (test-suite "strings_2.lua"))

; table.concat
; string.rep
(define (test-strings_3)
  (test-suite "strings_3.lua"))

; more table.concat
(define (test-strings_4)
  (test-suite "strings_4.lua"))

(provide test-strings_1 test-strings_2 test-strings_3 test-strings_4)