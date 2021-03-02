#lang racket
(require "./tests_aux.rkt")

; testing (parts of) table library
; table.unpack
; table.pack
(define (test-sort)
  (test-suite "sort.lua"
              (list "assert"
                    "next"
                    "pcall"
                    "print"
                    "select"
                    "table"
                    "table.unpack"
                    "table.pack"
                    )))
(provide test-sort)