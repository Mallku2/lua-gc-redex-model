#lang racket

(require redex
         (only-in parser-tools/lex lexer define-lex-abbrevs input-port
                  whitespace define-tokens define-empty-tokens lexeme
                  token-value)
         parser-tools/yacc
         (prefix-in re- parser-tools/lex-sre)
         math/flonum ; operations over flonums
         "../grammar.rkt")

; dictionary that maps alphabetic digits and their decimal interpretation,
; according to Lua's tonumber
(define letter_to_digit
  '(("a" . 10)
    ("A" . 10)

    ("b" . 11)
    ("B" . 11)

    ("c" . 12)
    ("C" . 12)

    ("d" . 13)
    ("D" . 13)

    ("e" . 14)
    ("E" . 14)

    ("f" . 15)
    ("F" . 15)

    ("g" . 16)
    ("G" . 16)

    ("h" . 17)
    ("H" . 17)

    ("i" . 18)
    ("I" . 18)

    ("j" . 19)
    ("J" . 19)

    ("k" . 20)
    ("K" . 20)

    ("l" . 21)
    ("L" . 21)

    ("m" . 22)
    ("M" . 22)

    ("n" . 23)
    ("N" . 23)

    ("o" . 24)
    ("O" . 24)

    ("p" . 25)
    ("P" . 25)

    ("q" . 26)
    ("Q" . 26)

    ("r" . 27)
    ("R" . 27)

    ("s" . 28)
    ("S" . 28)

    ("t" . 29)
    ("T" . 29)

    ("u" . 30)
    ("U" . 30)

    ("v" . 31)
    ("V" . 31)

    ("w" . 32)
    ("W" . 32)

    ("x" . 33)
    ("X" . 33)

    ("y" . 34)
    ("Y" . 34)

    ("z" . 35)
    ("Z" . 35)
    )
  )

; transform a given alpha-numeric character String into its decimal
; representation, according to base Number; returns nil if it is not
; possible
; PRE : {String contains one character}
(define-metafunction ext-lang
  char_to_digit : String Number -> any

  ; numeric digit
  [(char_to_digit String Number_1)
   Number_2

   (side-condition (char-numeric? (list-ref (string->list (term String)) 0)))
   (where Number_2 ,(string->number (term String)))
   (side-condition (> (term Number_1) (term Number_2)))]

  ; alphabetic digit
  [(char_to_digit String Number_1)
   Number_2

   (where Number_2 ,(with-handlers ([exn:fail? (λ (e) (term nil))])
                      ((λ ()
                         (dict-ref letter_to_digit (term String))))))
                         
   (side-condition (> (term Number_1) (term Number_2)))]

  ; default
  [(char_to_digit _ _)
   nil]
  )

; for a given String, it returns a list of decimal numbers, each one
; corresponding to the decimal representation of each character of
; String, according to base Number
(define-metafunction ext-lang
  string_to_digits : String Number -> any

  [(string_to_digits "" _)
   ()
   ]

  [(string_to_digits String_1 Number_1)
   (Number_2 Number_3 ...)

   ; get first character
   (where String_2 ,(substring (term String_1) 0 1))
   ; convert it
   (where Number_2 (char_to_digit String_2 Number_1))
   ; get suffix of the string
   (where String_3 ,(substring (term String_1) 1
                               (string-length (term String_1))))
   ; repeat with remaining digits
   (where (Number_3 ...) (string_to_digits String_3 Number_1))
   ]

  ; default
  [(string_to_digits _ _)
   nil]
  )

; receives a list of numbers, each one interpreted as the decimal
; representation of the digits of a number in base Number; it reconstructs
; the original number in its decimal representation
(define-metafunction ext-lang
  reconstruct_number : (Number ...) Number -> Number

  [(reconstruct_number () _)
   0.0]

  [(reconstruct_number (Number_1 Number_2 ...) Number_3)
   ; ops. over flonums to preserve precision
  ,(fl+ (fl* (term Number_4)
         (flexpt (term Number_5)
                 (fl- (real->double-flonum (length (term (Number_1 Number_2 ...))))
                      1.0)))
        (term (reconstruct_number (Number_2 ...) Number_3)))

  (where Number_4 ,(real->double-flonum (term Number_1)))
  (where Number_5 ,(real->double-flonum (term Number_3)))]
  )

; converts String into a decimal number, trying to interpret String as a
; number in base Number; returns nil if the conversion fails
(define-metafunction ext-lang
  convert_string : String Number -> any

  ; discard empty strings
  [(convert_string "" _)
   nil]

  ; negative number
  [(convert_string String_1 Number_1)
   ; Number_3 is guarantee to be a flonum
   ,(fl* (term Number_3) -1.0)

   ; it is a negative number
   (side-condition (equal? "-"
                           ; {length String_1 >= 1}
                           ; no need for exception catch
                           (substring (term String_1) 0 1)))
   ; get remaining characters
   (where String_2 ,(substring (term String_1) 1
                               (string-length (term String_1))))
   ; transform String into a list of decimal numbers, according to
   ; base Number_1
   (where (Number_2 ...) (string_to_digits String_2 Number_1))
   ; reconstruct number in decimal form, according to base Number_1
   (where Number_3 (reconstruct_number (Number_2 ...) Number_1))]

  ; not negative number, beginning with +
  [(convert_string String_1 Number_1)
   ; Number_3 is guarantee to be a flonum
   Number_3

   ; it is a negative number
   (side-condition (equal? "+"
                           ; {length String_1 >= 1}
                           ; no need for exception catch
                           (substring (term String_1) 0 1)))
   ; get remaining characters
   (where String_2 ,(substring (term String_1) 1
                               (string-length (term String_1))))
   ; transform String into a list of decimal numbers, according to
   ; base Number_1
   (where (Number_2 ...) (string_to_digits String_2 Number_1))
   ; reconstruct number in decimal form, according to base Number_1
   (where Number_3 (reconstruct_number (Number_2 ...) Number_1))]
  
  ; not negative number
  [(convert_string String Number_1)
   Number_3

   ; transform String into a list of decimal numbers, according to
   ; base Number_1
   (where (Number_2 ...) (string_to_digits String Number_1))
   ; reconstruct number in decimal form, according to base Number_1
   (where Number_3 (reconstruct_number (Number_2 ...) Number_1))]

  ; default: something went wrong
  [(convert_string _ _)
   nil]
  )

(provide convert_string)

; simple lexer that follows Lua's lexer conventions, in addition to allow an
; optional '+' prefixing positive numbers
(define-tokens non-empty-tokens (STRING NUMBER NAME))

(define-empty-tokens empty-tokens (EOF - +))

(define-lex-abbrevs
(digits (char-range "0" "9"))
  
  (hex-digits (re-or (char-range "0" "9")
                     (char-range "A" "F")
                     (char-range "a" "f")))

  (sign (re-? (re-or "+" "-")))
  ; decimal number, with optional fractional part
  (simp-number-lit (re-or
                    ; either no digit before . but at least one digit after
                    (concatenation sign
                                   (re-* digits)
                                   (re-? ".")
                                   (re-+ digits))

                    ; either no digit after . but at least one digit before
                    (concatenation sign
                                   (re-+ digits)
                                   (re-? ".")
                                   (re-* digits))))
  
  ; hexadecimal number, with optional fractional part
  (simp-hex-number-lit (re-or (concatenation (re-? "+")
                                             (re-or "0x" "0X")
                                             (re-+ hex-digits)
                                             (re-? ".")
                                             (re-* hex-digits))
                              (concatenation (re-? "+")
                                             (re-or "0x" "0X")
                                             (re-* hex-digits)
                                             (re-? ".")
                                             (re-+ hex-digits))))
  ; negative hexadecimal number, with optional fractional part, specified
  ; here to be able to give special treament to this case in the lexe
  (neg-simp-hex-number-lit (re-or (concatenation "-"
                                             (re-or "0x" "0X")
                                             (re-+ hex-digits)
                                             (re-? ".")
                                             (re-* hex-digits))
                              (concatenation "-"
                                             (re-or "0x" "0X")
                                             (re-* hex-digits)
                                             (re-? ".")
                                             (re-+ hex-digits))))
  
  ; decimal number, with optional fractional part and decimal exponent
  (scient-number-lit (concatenation simp-number-lit
                                    (re-or "e" "E")
                                    (re-or (concatenation (re-or "-" "+")
                                                          (re-+ digits))
                                           (re-+ digits))))
  
  ; hexadecimal number, with optional fractional part and binary exponent
  (hex-number-bin-exp-lit (concatenation simp-hex-number-lit
                                         (re-or "p" "P")
                                         (re-or (concatenation (re-or "-" "+")
                                                               (re-+ digits))
                                                (re-+ digits))))

  (neg-hex-number-bin-exp-lit (concatenation neg-simp-hex-number-lit
                                             (re-or "p" "P")
                                             (re-or (concatenation (re-or "-"
                                                                          "+")
                                                                   (re-+ digits))
                                                    (re-+ digits))))
  
  (number-lit (re-or simp-number-lit
                     scient-number-lit))
  
  (hex-number-lit (re-or simp-hex-number-lit
                         hex-number-bin-exp-lit))
  )


; Lexer for Lua
(define number-lexer
  (lexer ; literals
   ; exact->inexact, to use IEEE floating-point representation of a number,
   ; same as Lua
   (number-lit (token-NUMBER (real->double-flonum (string->number lexeme))))
   
   
   (simp-hex-number-lit
      (token-NUMBER (real->double-flonum
                     ; translate to Racket's hexadecimal numbers' notation,
                     ; remove + sign
                     (string->number (string-replace
                                      (string-replace lexeme
                                                     (regexp "0x|0X") "#x")
                                      (regexp "\\+") "")))))

   (neg-simp-hex-number-lit
      (token-NUMBER 
       (fl* -1.0
            (real->double-flonum
             ; translate to Racket's hexadecimal numbers' notation, and
             ; remove the leading - (not allowed in racket)
             (string->number (string-replace
                              (string-replace lexeme
                                              (regexp "0x|0X") "#x")
                              (regexp "\\-") ""))))))
   
   ; hex. with binary exp.
   (hex-number-bin-exp-lit
    (token-NUMBER ((lambda ()
                     (define hex-split (string-split lexeme (regexp "p|P")))
                               
                     (exact->inexact
                      (fl* (real->double-flonum
                            (string->number
                             (string-replace
                              (string-replace (list-ref hex-split 0)
                                              (regexp "0x|0X") "#x")
                              (regexp "\\+") "")))
                           
                         (flexpt 2.0
                                 (real->double-flonum
                                  (string->number (list-ref hex-split 1))))))))))

  (neg-hex-number-bin-exp-lit
    (token-NUMBER ((lambda ()
                     (define hex-split (string-split lexeme (regexp "p|P")))
                               
                     (exact->inexact
                      (fl* -1.0

                           (real->double-flonum
                            ; convert into racket's hexadecimal notation, remove
                            ; leading sign
                            (string->number (string-replace
                                             (string-replace (list-ref hex-split 0)
                                                             (regexp "0x|0X") "#x")
                                             (regexp "\\-") "")))
                           
                         (flexpt 2.0
                                 (real->double-flonum
                                  (string->number (list-ref hex-split 1))))))))))
    

   ; skip whitespaces
   (whitespace (number-lexer input-port))
   
   ((eof) (token-EOF))))

(provide number-lexer)

(define number-parser
  (parser
   
   ; Start symbol
   (start e)
   
   (end EOF)
   
   (error (lambda (tok-ok? tok-name tok-value)
            (error "Parser error. Token name:" tok-name)))

   ; Uncomment for debug
   ;(debug "log")
   
   ; Tokens declaration
   (tokens empty-tokens
           non-empty-tokens)
   
   (grammar
    
    (e ((NUMBER) $1)))
   
   ))

(provide number-parser)

(define (number-lex-this lexer input) (lambda () (lexer input)))

(define (number-parse-this input)
  (number-parser (number-lex-this number-lexer (open-input-string input))))

(provide number-parse-this)

; abbrevs for ext-number-lexer
(define-lex-abbrevs
  ; from ref. manual:
  ; the base may be any integer between 2 and 36, inclusive
  ; in bases above 10, the letter 'A' (in either upper or lower case)
  ; represents 10, 'B' represents 11, and so forth, with 'Z' representing 35
  (ext-digits (re-or (char-range "0" "9")
                 (char-range "A" "Z")
                 (char-range "a" "z")))
  
  ; decimal number, with optional fractional part
  (ext-number (concatenation (re-? (re-or "-" "+"))
                             (re-+ ext-digits))))
  
; simple lexer to clean strings for conversion purposes
(define ext-number-lexer
  (lexer
   ; we just return the token
   (ext-number (token-STRING lexeme))
   
   ; skip whitespaces
   (whitespace (ext-number-lexer input-port))
   ;("\t" (ext-number-lexer input-port))
   
   ((eof) (token-EOF))))

(provide ext-number-lexer)

(define ext-number-parser
  (parser
   
   ; Start symbol
   (start e)
   
   (end EOF)
   
   (error (lambda (tok-ok? tok-name tok-value)
            (error "Parser error. Token name:" tok-name)))

   ; Uncomment for debug
   ;(debug "log")
   
   ; Tokens declaration
   (tokens empty-tokens
           non-empty-tokens)
   
   (grammar
    
    (e ((STRING) $1)))
   
   ))

(provide ext-number-parser)

(define (ext-number-lex-this lexer input) (lambda () (lexer input)))
(define (ext-number-parse-this input base)
  (term
   ; first, we use the previous parser to verify that we received just a string
   ; with ext-digits and, possible, whitespaces, then we use convert_string over
   ; the resulting string
   (convert_string ,(ext-number-parser (ext-number-lex-this ext-number-lexer (open-input-string input)))
                   ,base))
  )

(provide ext-number-parse-this)