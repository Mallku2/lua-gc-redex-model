#lang racket

(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt"
         "./tests_aux.rkt")

(define (lua-gc-partial-test n)
  (partial_test n "gc" (plugIntoExecutionEnvironment services
                                                     ; TODO: let's make the parser fill this list
                                                     (list "assert"
                                                           "collectgarbage"
                                                           "next"
                                                           "pairs"
                                                           "setmetatable"
                                                           "type"
                                                           "string"
                                                           "string.rep")
                                                     (parse-this (file->string "gc.lua") #f (void)))))