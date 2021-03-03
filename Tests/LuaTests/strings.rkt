#lang racket
(require "./tests_aux.rkt")


; strings and string library
(define (test-strings_1)
  (test-suite "strings_1.lua"
              (services-from (file->string "strings_1.lua"))))

(define (test-strings_2)
  (test-suite "strings_2.lua"
              (services-from (file->string "strings_2.lua"))))

(define (test-strings_3)
  (test-suite "strings_3.lua"
              (services-from (file->string "strings_3.lua"))))

(provide test-strings_1 test-strings_2 test-strings_3)