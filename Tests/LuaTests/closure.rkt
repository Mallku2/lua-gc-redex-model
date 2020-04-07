#lang racket
(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (lua-closure-test-suite)
  (check-redundancy #t)
  (caching-enabled? #t)
  (test-predicate ok? (apply-reduction-relation*
                       full-progs-rel
                       (plugIntoExecutionEnvironment
                        services
                        ; TODO: let's make the parser fill this list
                        (list "assert"
                              "collectgarbage"
                              "error"
                              "pcall"
                              "print"
                              "math"
                              "math.sin")
                        (parse-this (file->string "closure.lua") #f (void)))))
  (test-results))


(define (lua-closure-partial-test n)
  (partial_test n "closure"
                (plugIntoExecutionEnvironment
                 services
                 ; TODO: let's make the parser fill this list
                 (list "assert"
                       "error"
                       "pcall"
                       "print"
                       "math"
                       "math.sin")
                 (parse-this (file->string "closure.lua") #f (void)))))
