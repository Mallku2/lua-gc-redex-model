#lang racket
(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (lua-constructs-test-suite)
    (test-predicate ok? (apply-reduction-relation*
                       full-progs-rel
                       (plugIntoExecutionEnvironment services
                                                     ; TODO: let's make the parser fill this list
                                                     (list "assert"
                                                           "collectgarbage"
                                                           "print"
                                                           "tonumber"
                                                           "type"
                                                           "math"
                                                           "math.sin")
                                                     (parse-this (file->string "constructs.lua") #f (void)))))
  (test-results))

(provide lua-constructs-test-suite)
