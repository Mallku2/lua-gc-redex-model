#lang racket
(require redex
         racket/serialize
         "../../grammar.rkt"
         "../../Relations/fullProgs.rkt"
         "../../executionEnvironment.rkt"
         "../../Desugar/parser.rkt")

(provide (all-defined-out))

; applies rel to term, through apply-reduction-relation, checking for the
; presence of non-determinism
(define steps 0)
(define (apply-reduction-relation-det rel aterm)
  (define terms (apply-reduction-relation rel aterm))
  (if (> (length terms) 1)
      (begin
        (println "non-determinism found:")
        (println aterm)
        (println "reduces to:")
        (println terms))
      (if (= (length terms) 0)
          (println aterm) ; aterm is a stuck term or last conf
          (begin
            (set! steps (add1 steps))
            (println steps)
            (apply-reduction-relation-det rel (first terms))))))

(define (apply-reduction-relation-n n rel term)
  (begin
    (define new_term (list))
    (define terms (list term))
    (for ([it (build-list n (lambda (x) x))]
          #:break (or (equal? terms '())
                      (> (length terms) 1)))
      (begin
        (set! new_term (list-ref terms 0))
        (set! terms (apply-reduction-relation rel new_term))
        (display it)
        (newline)
        ))
    
    (if (equal? terms '())

        (if (not (redex-match ext-lang
                              (σ : θ : \;)
                              new_term))

            #f

            terms)
        
        terms)
    )
  )

(define (set_new_partial_test term module_name)
  (begin
    (define out (open-output-file module_name))

    (write (serialize term) out)

    (close-output-port out)

    (newline))
  )

(define (partial_test n test_name term)
  (begin
    (define module_name (string-append test_name
                                       "_partial_test.rkt"))

    (define partial_result null)
    
    (if (file-exists? module_name)

        (set! partial_result (deserialize (read (open-input-file module_name))))

        (begin
          (set_new_partial_test term module_name)
          (set! partial_result term)))
    
    
    (check-redundancy #t)
    (caching-enabled? #t)
                 
    (define new_part_res
      (apply-reduction-relation-n n
                                  full-progs-rel
                                  partial_result))
    
    (if (or (not new_part_res)
            (> (length new_part_res) 1))
        ;Error
        new_part_res
    
        (if (equal? new_part_res '())
    
            "Finalized"
    
            (begin
              (delete-file module_name)
              (set_new_partial_test (list-ref new_part_res 0)
                                    module_name)
    
              "Not finalized")))
    ))

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