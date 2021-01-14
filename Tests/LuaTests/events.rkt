#lang racket
(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (lua-events-test-suite)
  (test-predicate ok? (apply-reduction-relation*
                       full-progs-rel
                       (plugIntoExecutionEnvironment services
                                                     ; TODO: let's make the parser fill this list
                                                     (list "_G"
                                                           "assert"
                                                           "collectgarbage"
                                                           "getmetatable"
                                                           "next"
                                                           "pairs"
                                                           "pcall"
                                                           "print"
                                                           "rawequal"
                                                           "rawget"
                                                           "rawlen"
                                                           "rawset"
                                                           "setmetatable"
                                                           "table"
                                                           "tostring"
                                                           "table.unpack"
                                                           "type"
                                                           )
                                                     (parse-this (file->string "events.lua") #f (void)))))
  (test-results))

(provide lua-events-test-suite)
