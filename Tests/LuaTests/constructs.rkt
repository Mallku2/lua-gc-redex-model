#lang racket
(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt")

(define (ok? red)
  (and (eq? (length red) 1)

       (redex-match core-lang
              (σ : θ : \;)
              (first red))))

(define (lua-constructs-test-suite)
    (test-predicate ok? (apply-reduction-relation*
                       full-progs-rel
                       (plugIntoExecutionEnvironment services
                                                     ; TODO: let's make the parser fill this list
                                                     (list "assert"
                                                           "print"
                                                           "tonumber"
                                                           "type"
                                                           "math"
                                                           "math.sin")
                                                     (parse-this (file->string "constructs.lua") #f (void)))))
  (test-results))

(provide lua-constructs-test-suite)
