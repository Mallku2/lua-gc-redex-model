#lang racket

(require redex
         "wellFormedGrammar.rkt")

(define-extended-language ext-lang well-formed-lang)

(provide ext-lang)