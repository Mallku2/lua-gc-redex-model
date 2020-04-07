#lang racket
(require redex
         "../grammar.rkt")

(define (evaluation-contexts-test-suite)

  ; Elf are all the possible evaluation contexts, with no labelled blocks 
  (test-predicate (redex-match ext-lang Elf)
                  (term (do hole end)))

  (test-predicate (redex-match ext-lang Elf)
                  (term ((hole)ProtectedMode)))

  (test-predicate (redex-match ext-lang Elf)
                  (term (((do hole end))ProtectedMode)))

  (test-predicate (redex-match ext-lang Elf)
                  (term (do ((hole)ProtectedMode) end)))

  (test-predicate (lambda (t)
                    (not ((redex-match ext-lang Elf) t)))
                  (term (:: X :: \{ hole \})))

  ; El are all the possible evaluation contexts, with no protected mode 
  (test-predicate (redex-match ext-lang El)
                  (term (do hole end)))

  (test-predicate (redex-match ext-lang El)
                  (term (:: X :: \{ hole \})))

  (test-predicate (redex-match ext-lang El)
                  (term (:: X :: \{ (do hole end) \})))

  (test-predicate (redex-match ext-lang El)
                  (term (do (:: X :: \{ hole \}) end)))

  (test-predicate (lambda (t)
                    (not ((redex-match ext-lang El) t)))
                  (term ((hole)ProtectedMode)))

  (test-results)
)

(provide evaluation-contexts-test-suite)
