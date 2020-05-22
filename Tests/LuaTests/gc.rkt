#lang racket

(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (lua-gc-test-suite)
  (check-redundancy #t)
  (caching-enabled? #t)
  (test-predicate ok? (apply-reduction-relation*
                       full-progs-rel
                       (plugIntoExecutionEnvironment
                        services
                        ; TODO: let's make the parser fill this list
                        (list "assert"
                              "collectgarbage"
                              "load"
                              "next"
                              "pairs"
                              "pcall"
                              "print"
                              "setmetatable"
                              "type"
                              "string"
                              "string.len"
                              "string.rep"
                              "string.sub")
                        (parse-this (file->string "gc.lua") #f (void))))))

(define (lua-gc-partial-test n)
  (partial_test n "gc"
                (plugIntoExecutionEnvironment
                 services
                 (list "assert"
                       "collectgarbage"
                       "next"
                       "pairs"
                       "setmetatable"
                       "type"
                       "string"
                       "string.rep")
                 (parse-this (file->string "gc.lua") #f (void)))))