#lang racket
(require redex
         "../grammar.rkt")
; Predicates over grammar's symbols, to ease the definition of some rules
; from the model
(define-metafunction ext-lang
  [(isArithBinOp +)
   #t]
  
  [(isArithBinOp -)
   #t]
  
  [(isArithBinOp *)
   #t]
  
  [(isArithBinOp %)
   #t]
  
  [(isArithBinOp ^)
   #t]
  
  [(isArithBinOp /)
   #t]
  
  [(isArithBinOp any)
   #f])

(provide isArithBinOp)


(define-metafunction ext-lang
  [(isRelationalOperator <)
   #t]
  
  [(isRelationalOperator <=)
   #t]
  
  [(isRelationalOperator any)
   #f])

(provide isRelationalOperator)


(define-metafunction ext-lang
  [(isNumberBinOp binop)
   (or (isArithBinOp binop)
       (isRelationalOperator binop))])

(provide isNumberBinOp)


(define-metafunction ext-lang
  [(translateComparisonOp >)
   <]
  
  [(translateComparisonOp >=)
   <=])

(provide translateComparisonOp)

(define-metafunction ext-lang
  [(isBooleanBinOp and)
   #t]
  
  [(isBooleanBinOp or)
   #t]
  
  [(isBooleanBinOp any)
   #f])

(provide isBooleanBinOp)


(define-metafunction ext-lang
  [(isStringBinOp binop)
   (or (term (isRelationalOperator binop))
       (equal? (term binop) (term ..)))])

(provide isStringBinOp)


; For each ev. contexts where tuples are truncated, we need to add an equation
; this definition
(define-metafunction ext-lang
  [(fixUnwrap (return v_1 ... hole) (v_2 ...))
   (return v_1 ... v_2 ...)]

  [(fixUnwrap (return v_1 ... hole) '())
   (return v_1 ...) ]
  
  [(fixUnwrap (v_1 (v_2 ... hole)) (v_3 ...))
   (v_1 (v_2 ... v_3 ...))]

  [(fixUnwrap (v_1 (v_2 ... hole)) '())
   (v_1 (v_2 ...))]

  [(fixUnwrap ($statFunCall v_1 (v_2 ... hole)) (v_3 ...))
   ($statFunCall v_1 (v_2 ... v_3 ...))]

  [(fixUnwrap ($statFunCall v_1 (v_2 ... hole)) '())
   ($statFunCall v_1 (v_2 ...))]
  
  [(fixUnwrap ($builtIn builtinserv  (v_1 ... hole)) (v_2 ...))
   ($builtIn builtinserv  (v_1 ... v_2 ...))]

  [(fixUnwrap ($builtIn builtinserv  (v_1 ... hole)) '())
   ($builtIn builtinserv  (v_1 ...))]
  
  [(fixUnwrap (< v_1 ... hole >) (v_2 ...))
   (< v_1 ... v_2 ... >)]

  [(fixUnwrap (< v_1 ... hole >) '())
   (< v_1 ... >)]
  
  [(fixUnwrap (\{ field ... hole \}) (v ...))
   (\{ field ... v ... \})]
  
  [(fixUnwrap (\{ field ... hole \}) '())
   (\{ field ... \})]

  [(fixUnwrap (local Name ... = v_1 ... hole in s end) (v_2 ...))
   (local Name ... = v_1 ... v_2 ... in s end)]

  [(fixUnwrap (local Name ... = v_1 ... hole in s end) '())
   (local Name ... = v_1 ... in s end)]

  [(fixUnwrap (evar ... = v_1 ... hole) (v_2 ...))
   (evar ... = v_1 ... v_2 ...)]

  [(fixUnwrap (evar ... = v_1 ... hole) '())
   (evar ... = v_1 ...)]
  )

(provide fixUnwrap)

; To flat a list of symbols into an string representing the corresponding
; s-expression (needed for the implementation of string.dump).
(define (str-flatten l)
  (string-append
   "("
   (foldr
    (lambda (elem acum)
      (if (symbol? elem)
          ; Some special treatment for brackets, curly braces, etc
          (cond
            [(equal? elem '\[)
             (string-append "\\[" " " acum)]
            [(equal? elem '\])
             (string-append "\\]" " " acum)]
            [(equal? elem '\{)
             (string-append "\\{" " " acum)]
            [(equal? elem '\})
             (string-append "\\}" " " acum)]
            [(equal? elem '\;)
             (string-append "\\;" " " acum)]
            [(equal? elem '\#)
             (string-append "\\#" " " acum)]
            ; No problem.
            [else (string-append (symbol->string elem) " " acum)])
          ; Other special cases: strings and numbers
          (cond [(string? elem)
                 (string-append (string-append "\"" elem "\"") " " acum)]
                [(number? elem)
                 (string-append (number->string elem) " " acum)]
                ; Then, it should by a nested structure. Recursive call.
                [else (string-append (str-flatten elem) " " acum)]
                )))
    ")"
    l)))

(provide str-flatten)

; simple solution to the problem of pluging a concat of stats into another
; concat of stats, obtaining a well-formed concat of stats
(define-metafunction ext-lang
  concat-stats : E any -> s

  [(concat-stats (in-hole E (hole scoresing_1 scoresing_2 ...))
                 (ssing scoresing_4 scoresing_5 ...))
   (in-hole E (ssing scoresing_4 scoresing_5 ... scoresing_1 scoresing_2 ...))]

  [(concat-stats E any)
   (in-hole E any)]
  )

(provide concat-stats)