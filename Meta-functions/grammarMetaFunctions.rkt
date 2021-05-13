#lang racket
(require redex
         "../grammar.rkt")

(define-metafunction ext-lang
  [(trans_comp_op >)
   <]
  
  [(trans_comp_op >=)
   <=])

(provide trans_comp_op)

; For each ev. contexts where tuples are truncated, we need to add an equation
; this definition
(define-metafunction ext-lang
  [(fix_unwrap (return v_1 ... hole) (v_2 ...))
   (return v_1 ... v_2 ...)]

  [(fix_unwrap (return v_1 ... hole) '())
   (return v_1 ...) ]
  
  [(fix_unwrap (v_1 (v_2 ... hole)) (v_3 ...))
   (v_1 (v_2 ... v_3 ...))]

  [(fix_unwrap (v_1 (v_2 ... hole)) '())
   (v_1 (v_2 ...))]

  [(fix_unwrap ($statFCall v_1 (v_2 ... hole)) (v_3 ...))
   ($statFCall v_1 (v_2 ... v_3 ...))]

  [(fix_unwrap ($statFCall v_1 (v_2 ... hole)) '())
   ($statFCall v_1 (v_2 ...))]
  
  [(fix_unwrap ($builtIn builtinserv  (v_1 ... hole)) (v_2 ...))
   ($builtIn builtinserv  (v_1 ... v_2 ...))]

  [(fix_unwrap ($builtIn builtinserv  (v_1 ... hole)) '())
   ($builtIn builtinserv  (v_1 ...))]
  
  [(fix_unwrap (< v_1 ... hole >) (v_2 ...))
   (< v_1 ... v_2 ... >)]

  [(fix_unwrap (< v_1 ... hole >) '())
   (< v_1 ... >)]
  
  [(fix_unwrap (\{ field ... hole \}) (v ...))
   (\{ field ... v ... \})]
  
  [(fix_unwrap (\{ field ... hole \}) '())
   (\{ field ... \})]

  [(fix_unwrap (local Name ... = v_1 ... hole in s end) (v_2 ...))
   (local Name ... = v_1 ... v_2 ... in s end)]

  [(fix_unwrap (local Name ... = v_1 ... hole in s end) '())
   (local Name ... = v_1 ... in s end)]

  [(fix_unwrap (evar ... = v_1 ... hole) (v_2 ...))
   (evar ... = v_1 ... v_2 ...)]

  [(fix_unwrap (evar ... = v_1 ... hole) '())
   (evar ... = v_1 ...)]
  )

(provide fix_unwrap)

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
            [(equal? elem '\()
             (string-append "\\(" " " acum)]
            [(equal? elem '\))
             (string-append "\\)" " " acum)]
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
  concat_stats : E any -> t

  [(concat_stats (in-hole E (hole sing_1 sing_2 ...))
                 (sing sing_4 sing_5 ...))
   (in-hole E (sing sing_4 sing_5 ... sing_1 sing_2 ...))]

  [(concat_stats E any)
   (in-hole E any)]
  )

(provide concat_stats)



;                                                                                            
;                                                                                            
;                                                                                            
;                                       ;                     ;;;                            
;                       ;               ;                    ;                               
;                       ;               ;                    ;                               
;   ;;;;;;     ;;;;   ;;;;;;     ;;;    ; ;;;;             ;;;;;;  ;     ;  ; ;;;;     ;;;   
;   ;  ;  ;   ;    ;    ;       ;   ;   ;;   ;;              ;     ;     ;  ;;   ;;   ;   ;  
;   ;  ;  ;        ;    ;      ;        ;     ;              ;     ;     ;  ;     ;  ;       
;   ;  ;  ;   ;;;;;;    ;      ;        ;     ;              ;     ;     ;  ;     ;  ;       
;   ;  ;  ;  ;;    ;    ;      ;        ;     ;              ;     ;     ;  ;     ;  ;       
;   ;  ;  ;  ;     ;    ;      ;        ;     ;              ;     ;     ;  ;     ;  ;       
;   ;  ;  ;  ;    ;;    ;       ;   ;   ;     ;              ;     ;;   ;;  ;     ;   ;   ;  
;   ;  ;  ;   ;;;; ;     ;;;     ;;;    ;     ;              ;      ;;;; ;  ;     ;    ;;;   
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
                                                                             

(define is_s?
  (redex-match? ext-lang
                s))

(define is_e?
  (redex-match? ext-lang
                e))

(define (is_term? t)
  (redex-match? ext-lang
                t))


; values
(define is_v?
  (redex-match? ext-lang
                v))

(define is_number?
  (redex-match? ext-lang
                Number))

(define is_string?
  (redex-match? ext-lang
                String))

(define is_nil?
  (redex-match? ext-lang
                nil))

(define is_false?
  (redex-match? ext-lang
                false))

; values that are interpreted as false, in a boolean context
(define (is_false_cond? t)
  (or (is_false? t)
      (is_nil? t)))

(define is_true?
  (redex-match? ext-lang
                true))

(define (is_bool? t)
  (or (is_false? t)
      (is_true? t)))

(define (is_tid? t)
  (redex-match? ext-lang
                tid
                t))

(define is_cid?
  (redex-match? ext-lang
                cid))

(define is_fdef?
  (redex-match? ext-lang
                functiondef))


; operators
(define is_strconcat?
  (redex-match? ext-lang
                ..))

(define is_arithop?
  (redex-match? ext-lang
                arithop))

(define is_and?
  (redex-match? ext-lang
                and))

(define is_or?
  (redex-match? ext-lang
                or))

(define (is_bool_binop? t)
  (or (is_and? t)
      (is_or? t)))

(define is_relop?
  (redex-match? ext-lang
                relop))

(define is_eq?
  (redex-match? ext-lang
                ==))

(define is_lt?
  (redex-match? ext-lang
                <))

(define is_le?
  (redex-match? ext-lang
                <=))

(define is_gt?
  (redex-match? ext-lang
                >))

(define is_ge?
  (redex-match? ext-lang
                >=))

(define (is_lt_le? t)
  (or (is_lt? t)
      (is_le? t)))

(define (is_gt_ge? t)
  (or (is_gt? t)
      (is_ge? t)))

; exps
(define is_r?
  (redex-match? ext-lang
                r))

; statements
(define is_skip?
  (redex-match? ext-lang
                \;))

(define is_break?
  (redex-match? ext-lang
                break))

; state
(define is_intreptable?
  (redex-match? ext-lang
                intreptable))

(define (is_cte? t)
  (or (is_tid? t)

      (is_cid? t)))

(define is_theta?
  (redex-match? ext-lang
                θ))

(define is_conf?
  (redex-match? ext-lang
                (σ : θ : s)))


(provide is_s? is_e? is_term?
         ;values
         is_v?
         is_number?
         is_string?
         is_tid?
         is_cid?
         is_fdef?
         is_nil?
         is_false?
         is_true?
         is_false_cond?
         is_bool?

         ; ops
         is_strconcat?
         is_arithop?
         is_and?
         is_or?
         is_bool_binop?
         is_eq?
         is_relop?
         is_lt?
         is_gt?
         is_le?
         is_ge?
         is_lt_le?
         is_gt_ge?

         ; stats
         is_skip?
         is_break?
         
         is_r?
         is_intreptable? 
         is_cte?
         is_theta?
         is_conf? )