#lang racket
(require redex
         ; Meta-functions test suites
         "./deltaTests.rkt"
         "./objStoreMetaFunctionsTests.rkt"
         "./substitutionTests.rkt"
         "./gcTests.rkt"
         ; Notions of reduction test suites
         "./termsTests.rkt"
         "./termsValStoreTests.rkt"
         "./termsObjStoreTests.rkt"
         "./termsValObjStoreTests.rkt"
         "./metaTests.rkt"
         "./fullProgsTests.rkt"
         "./executionEnvironmentTests.rkt"
         ; Random testing
         "./RandomTesting/soundness/wfc_test_suite.rkt"
         ; Desugaring
         "../Desugar/parser_tests.rkt"
         "../Desugar/lexer_tests.rkt"
         "../Desugar/phrases_constructors_tests.rkt"
         rackunit/text-ui
         )

(define (test-all-metafunctions)
  (print "delta-test-suite :")
  (delta-test-suite)
  (print "obj-store-metafunctions-test-suite :")
  (test-all-obj-store-metafunctions-suites)
  (print "substitution-test-suite :")
  (subs-test-suite)
  (print "gc-test-suite :")
  (gc-test-suite)
  (print "test-all-meta-table-mech-metafunctions-test-suites: ")
  (test-all-meta-table-mech-metafunctions-test-suites)
  )

(define (test-all-reductions)
  (print "terms-rel-test-suite: ")
  (terms-rel-test-suite)
  (print "terms-val-store-test-suite:")
  (terms-val-store-test-suite)
  (print "terms-obj-store-red-test-suite:")
  (terms-obj-store-test-suite)
  (print "terms-val-obj-store-red-test-suite:")
  (terms-val-obj-store-test-suite)
  (print "meta:")
  (meta-test-suite)
  (print "full-progs-rel-test-suite: ")
  (full-progs-rel-test-suite)
  (print "standard-library-test-suite: ")
  (standard-library-test-suite)
  )

(define (test-all-random-tests)
  (print "random testing:")
  (print "well-formedness:")
  (well-formed-test-suite)
  (print "free occurrences of references:")
  (free-refs-test-suite)
  )

(define (test-all-desugaring-tests)
  (print "lexer tests:")
  (run-tests lexer-test-suite)
  (print "parser tests:")
  (run-tests parser-test-suite)
  (print "concrete grammar rep tests:")
  (run-tests phrases-constructors-test-suite)  
  )

(define (test-all)
  (check-redundancy #t)
  (caching-enabled? #t)
  (test-all-metafunctions)
  (test-all-random-tests)
  (test-all-desugaring-tests)
  (test-all-reductions))

(test-all)
