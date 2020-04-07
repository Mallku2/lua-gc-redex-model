#lang racket
(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt"
         "../RandomTesting/progress/defs.rkt")

(define (lua-calls-test-suite)
  (check-redundancy #t)
  (caching-enabled? #t)
  (test-predicate ok? (apply-reduction-relation*
                       full-progs-rel
                       (plugIntoExecutionEnvironment
                        services
                        ; TODO: let's make the parser fill this list
                        (list "collectgarbage"
                              "error"
                              "print"
                              "assert"
                              "type"
                              "load"
                              "rawget"
                              "rawset"
                              "string"
                              "pcall"
                              "string.dump"
                              "string.sub"
                              "table"
                              "table.pack"
                              "math"
                              "math.sin")
                        (parse-this (file->string "calls.lua") #f (void)))))
  (test-results))

(provide lua-calls-test-suite)

(define (lua-calls-partial-test n)
  (check-redundancy #t)
  (caching-enabled? #t)
  (partial_test n "calls" (plugIntoExecutionEnvironment services
                                                        ; TODO: let's make the parser fill this list
                                                        (list "collectgarbage"
                                                              "error"
                                                              "print"
                                                              "assert"
                                                              "type"
                                                              "load"
                                                              "rawget"
                                                              "rawset"
                                                              "string"
                                                              "pcall"
                                                              "string.dump"
                                                              "string.sub"
                                                              "table"
                                                              "table.pack"
                                                              "math"
                                                              "math.sin")
                                                        (parse-this (file->string "calls.lua") #f (void)))))