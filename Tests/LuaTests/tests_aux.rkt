#lang racket
(require redex
         racket/serialize
         "../../grammar.rkt"
         "../../Relations/fullProgs.rkt"
         "../../executionEnvironment.rkt"
         "../../Desugar/parser.rkt")

(provide (all-defined-out))

; test for proper well-formed final configuration and unique result
(define (ok? red)
  (and (eq? (length red) 1)
       (or (redex-match? ext-lang
                         (σ : θ : \;)
                         (term ,(first red)))

           (redex-match? ext-lang
                         (side-condition
                          (σ : θ : (in-hole E (return v ...)))
                          (not (or (redex-match? ext-lang
                                                 (in-hole E_2 ((in-hole Elf hole)
                                                               (renv ...) RetStat))
                                                 (term E))
                                   
                                   (redex-match? ext-lang
                                                 (in-hole E_2 ((in-hole Elf hole)
                                                               (renv ...) RetExp))
                                                 (term E))
                                   
                                   (redex-match? ext-lang
                                                 (in-hole E_2 ((in-hole Elf hole)
                                                               Break))
                                                 (term E)))))
                         (term ,(first red))))
       ))

(provide ok?)

(define (full_term file tests_services)
  (plugIntoExecutionEnvironment
                        services
                        tests_services
                        (parse-this (file->string file) #f (void))))

(provide full_term)

(define (test-suite file)
  (printf "Testing ~V\n" file)
  (test-predicate ok? (apply-reduction-relation*
                       full-progs-rel
                       (full_term file
                                  ; library services from file
                                  (services-from (file->string file)))))
  (printf "File ~V passed\n" file))

(provide test-suite)

(define (services-from str)
  (filter (lambda (x) (string-contains? str x)) (dict-keys services)))
(provide services-from)