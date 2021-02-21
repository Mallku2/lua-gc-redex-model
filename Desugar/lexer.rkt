#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

(provide (all-defined-out))

(provide token-value)

(provide token-name)

(define-empty-tokens empty-tokens (EOF
                                   ; statements
                                   \;
                                   :
                                   BREAK
                                   RETURN
                                   REPEAT
                                   UNTIL
                                   IF
                                   THEN
                                   ELSE
                                   ELSEIF
                                   END
                                   DO
                                   WHILE
                                   FOR
                                   IN
                                   LOCAL
                                   FUNCTION
                                   =
                                   \[
                                   \]
                                   \(
                                   \)
                                   \{
                                   \}
                                   \.
                                   \,
                                   ; expressions
                                   +
                                   -
                                   *
                                   /
                                   %
                                   ^
                                   CONCAT
                                   LT
                                   LE
                                   GT
                                   GE
                                   EQ
                                   NOTEQ
                                   AND
                                   OR
                                   NOT
                                   \#
                                   NIL
                                   FALSE
                                   TRUE
                                   VARARG
                                   ; tokens for resolving shif-reduce conflicts
                                   BEGINNING_DO_END
                                   BEGINNING_WHILE
                                   BEGINNING_REPEAT
                                   UNM ; arith. negation
                                   ))

(define-tokens non-empty-tokens (STRING NUMBER NAME))

(define-lex-abbrevs
  ; TODO: fix re for strings
  (double-quoted-str (re-: "\""
                           (re-* (char-complement #\"))
                           "\""))

  ; strings in long brackets
  (long-bracket-str-beg (re-: "["
                              (re-* "=")
                              "["
                              ))

  ; from ref. manual: "For convenience, when the opening long bracket is
  ; immediately followed by a newline, the newline is not included in the string"
  ; we discard the newline char with this abbrev.
  (long-bracket-str-beg-newline (re-: "["
                                      (re-* "=")
                                      "["
                                      "\n"))

  (long-bracket-str-end (re-: "]"
                              (re-* "=")
                              "]"))

  (not-long-bracket-str-end (re-or (re-~ "]")

                                   (re-: "]"
                                         (re-* "="))
                                          
                                   (re-: "]"
                                         (re-* "=")
                                         (re-~ "]"))))
  
  (single-quoted-str (re-: "'"
                                  (re-* (char-complement #\'))
                                  "'"))
  
  (digits (char-range "0" "9"))
  
  (hex-digits (re-or (char-range "0" "9")
                     (char-range "A" "F")
                     (char-range "a" "f")))
  
  ; Decimal number, with optional fractional part
  (simp-number-lit (re-or
                    ; either no digit before . but at least one digit after
                    (concatenation (re-* digits)
                                   (re-? ".")
                                   (re-+ digits))

                    ; either no digit after . but at least one digit before
                    (concatenation (re-+ digits)
                                   (re-? ".")
                                   (re-* digits))))
  
  ; Hexadecimal number, with optional fractional part
  (simp-hex-number-lit (concatenation (re-or "0x" "0X")
                                      (re-* hex-digits)
                                      (re-? ".")
                                      (re-* hex-digits)))
  
  ; Decimal number, with optional fractional part and decimal exponent
  (scient-number-lit (concatenation simp-number-lit
                                    (re-or "e" "E")
                                    (re-or (concatenation (re-or "-" "+")
                                                          (re-+ digits))
                                           (re-+ digits))))
  
  ; Hexadecimal number, with optional fractional part and binary exponent
  (hex-number-bin-exp-lit (concatenation simp-hex-number-lit
                                         (re-or "p" "P")
                                         (re-or (concatenation (re-or "-" "+")
                                                               (re-+ digits))
                                                (re-+ digits))))
  
  (number-lit (re-or simp-number-lit
                     scient-number-lit))
  
  (hex-number-lit (re-or simp-hex-number-lit
                         hex-number-bin-exp-lit))
  
  (id-beg-chars (re-or (char-range "a" "z")
                       (char-range "A" "Z")
                       "_"))
  
  (id (concatenation id-beg-chars
                     (re-* (re-or id-beg-chars
                                  "_"
                                  digits)))))

; Lexer for Lua
(define lua-lexer
  (lexer ; literals
   ("nil" (token-NIL))
   ("false" (token-FALSE))
   ("true" (token-TRUE))
   ; exact->inexact, to use IEEE floating-point representation of a number,
   ; same as Lua
   (number-lit (token-NUMBER (exact->inexact (string->number lexeme))))
   
   ; Translate to Racket's hexadecimal numbers' notation
   (simp-hex-number-lit
    (token-NUMBER (string->number (string-replace lexeme
                                                  (regexp "0x|0X") "#x"))))
   
   ; hex. with binary exp.
   (hex-number-bin-exp-lit
    (token-NUMBER
     ((lambda ()
        (define hex-split (string-split lexeme (regexp "p|P")))
                               
        (exact->inexact
         (* (string->number (string-replace (list-ref hex-split 0)
                                            (regexp "0x|0X") "#x"))
            (expt 2 (string->number (list-ref hex-split 1))))))))
    )
   
   (double-quoted-str
    (token-STRING
     ; Remove unprintable characters
     ; (embedded zeros)
     (clean (substring lexeme
                       1
                       (- (string-length lexeme) 1)))))
   
   (single-quoted-str
    (token-STRING
     ; Remove unprintable characters
     ; (embedded zeros)
     (clean (substring lexeme
                       1
                       (- (string-length lexeme) 1)))))

   (long-bracket-str-beg
    (begin
      (define res
        ((lua-long-bracket-str (- (string-length lexeme) 2)) input-port))
      (if (token? res)
          ; Remove unprintable characters
          ; (embedded zeros)
          (token-STRING (clean (token-value res)))
          ; res is an error
          res)))

   (long-bracket-str-beg-newline
    (begin
      (define res
        ((lua-long-bracket-str (- (string-length lexeme) 3)) input-port))
      (if (token? res)
          ; Remove unprintable characters
          ; (embedded zeros)
          (token-STRING (clean (token-value res)))
          res)))
   
   
   ("..." (token-VARARG))
   (".." (token-CONCAT))
   (";" (token-\;))
   (":" (token-:))
   ("," (token-\,))
   ("." (token-\.))
   ; Delete comments
   ("--[[" (lua-mult-line-comment-lexer input-port))
   ("--" (lua-sing-line-comment-lexer input-port))
   
   ("-" (token--))
   ("+" (token-+))
   ("*" (token-*))
   ("/" (token-/))
   ("^" (token-^))
   ("%" (token-%))
   ("<" (token-LT))
   ("<=" (token-LE))
   (">" (token-GT))
   (">=" (token-GE))
   ("==" (token-EQ))
   ("~=" (token-NOTEQ))
   ("and" (token-AND))
   ("or" (token-OR))
   ("not" (token-NOT))
   ("#" (token-\#))
   ("=" (token-=))
   ("[" (token-\[))
   ("]" (token-\]))
   ("(" (token-\())
   (")" (token-\)))
   ("{" (token-\{))
   ("}" (token-\}))
   ("break" (token-BREAK))
   ("return" (token-RETURN))
   ("if" (token-IF))
   ("then" (token-THEN))
   ("else" (token-ELSE))
   ("elseif" (token-ELSEIF))
   ("do" (token-DO))
   ("end" (token-END))
   ("while" (token-WHILE))
   ("for" (token-FOR))
   ("repeat" (token-REPEAT))
   ("until" (token-UNTIL))
   ("in" (token-IN))
   ("local" (token-LOCAL))
   ("function" (token-FUNCTION))
   
   ; identifiers
   (id (token-NAME (string->symbol lexeme)))
   
   ; skip whitespaces
   (whitespace (lua-lexer input-port))
   
   ((eof) (token-EOF))))

; another lexer, specific for skipping comments' content
; single-line comments
(define lua-sing-line-comment-lexer 
  (lexer
   ["\n" (lua-lexer input-port)]
   
   [any-char (lua-sing-line-comment-lexer input-port)]))

; another lexer, specific for skipping comments' content
; multiple-lines comments
(define lua-mult-line-comment-lexer 
  (lexer
   ["]]--" (lua-lexer input-port)]
   
   [any-char
    (lua-mult-line-comment-lexer input-port)]))

; strings in long brackets; receives the quantity eq_quant of = that
; the closing brackets must contain
; for simplicity, the function ends returning a token-STRING or an error, without
; passing explicit control to lua-lexer 
(define (lua-long-bracket-str eq_quant)
  (lexer
    ; consume as much chars as possible, before closing long brackets
   [not-long-bracket-str-end
    (begin
      (define res ((lua-long-bracket-str eq_quant) input-port))
      (if (token? res)
          ; res is a token: append lexeme to its value
          (token-STRING
           ; TODO: "Any kind of end-of-line sequence (carriage return,
           ; newline, carriage return followed by newline, or newline followed
           ; by carriage return) is converted to a simple newline"
           (clean (string-append lexeme (token-value res))))
          ; res is an error
          res)
      )]
   
   [long-bracket-str-end
    (if (= (- (string-length lexeme) 2) eq_quant)
        ; we found the end of the string: we return the empty string to handle
        ; the case of an empty string: [[]]
        (token-STRING "")
        ; not the expected long bracket: we need to append it to the result
        ((lambda (res)
           (if (token? res)
               ; res is a token: append lexeme to its value
               (token-STRING
                (clean (string-append lexeme (token-value res))))
               ; res is an error
               res)) ((lua-long-bracket-str eq_quant) input-port))
        )]
   ))

; racket's strings "prints using doublequotes, where doublequote and backslash
; characters within the string are escaped with backslashes"
; (from https://docs.racket-lang.org/guide/strings.html), which is not the case
; with Lua's string
; we implement this procedure to eliminate redundant escape characters
(define (clean string)
  (define aux string)
  
  ; Unescape backslashs from \n
  (set! aux (string-replace aux "\\n" "\n"))
  (set! aux (string-replace aux "\\\n" "\n"))
  ; Unescape backslashs from \0
  (set! aux (string-replace aux "\\0" "\0"))
  (set! aux (string-replace aux "\\\0" "\0"))
  ; Unescape backslashs from \r
  (set! aux (string-replace aux "\\r" "\r"))
  (set! aux (string-replace aux "\\\r" "\r"))
    
  (if (equal? aux string)
      string ; Nothing left to be cleaned
      (clean aux))
  )



(provide lua-lexer)