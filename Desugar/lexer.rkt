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

  ; some lexical conventions of strings:
  (string-delimiter (re-or "'"
                           "\""))
  
  ; ref. manual: "\ddd, where ddd is a sequence of up to three decimal digits"
  (byte-decimal (re-or (re-: "\\"
                             digits
                             digits
                             digits)
                       
                       (re-: "\\"
                             digits
                             digits)
                       
                       (re-: "\\"
                             digits)))

  ; ref. manual: "\xXX, where XX is a sequence of exactly two hexadecimal digits"
  (byte-hexadecimal (re-: "\\x"
                          hex-digits
                          hex-digits))

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

  (not-long-bracket-str-end (re-& (re-~ "]")
                                  (re-~ "=")))
  
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

   ; from ref. manual: "String represents immutable sequences of bytes."
   ; however, in order to maintain the same syntax as in Lua, we preserve
   ; racket's strings until the moment of operations on them, where we
   ; translate them to bytes/utf-8
   (string-delimiter ((lua-strings lexeme (position-line start-pos)) input-port))

   (long-bracket-str-beg
    (begin
      ; set flags and accums
      (set! cons-long-bracket-end #f)
      (set! long-bracket-end "")
      (define res
        ; compute quantity of =
        ((lua-long-bracket-str (- (string-length lexeme) 2)) input-port))
      (if (token? res)
          (token-STRING (clean (token-value res)))
          ; res is an error
          res)))

   (long-bracket-str-beg-newline
    (begin
      ; set flags and accums
      (set! cons-long-bracket-end #f)
      (set! long-bracket-end "")
      (define res
        ; compute quantity of =
        ((lua-long-bracket-str (- (string-length lexeme) 3)) input-port))
      (if (token? res)
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
   ("--" ((lua-sing-line-comment-lexer (position-line start-pos)) input-port))
   
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
(define (lua-sing-line-comment-lexer actual_line)
  (lexer
;   ["\n" (if (not (= (position-line end-pos) actual_line))
;             (lua-lexer input-port)
;             ((lua-sing-line-comment-lexer actual_line) input-port))]

   [(eof)
    (lua-lexer input-port)]
   
   [any-char
    (if (not (= (position-line end-pos) actual_line))
        ; ending
        (lua-lexer input-port)
        ((lua-sing-line-comment-lexer actual_line) input-port))]))

; another lexer, specific for skipping comments' content
; multiple-lines comments
(define lua-mult-line-comment-lexer 
  (lexer
   ["]]--" (lua-lexer input-port)]

   [(eof) (lua-lexer input-port)]
   
   [any-char
    (lua-mult-line-comment-lexer input-port)]))

; lexer that implements Lua's strings lexical conventions
; PARAM:
; closing_char: the single or double quote char (contained into a string) that
; delimits the string
; actual_line: line where the string begins
; RETURNS:
; a token-STRING or error
(define (lua-strings closing_char actual_line)
  (lexer
   ; byte specified with decimal numbers
   [byte-decimal

    (if (and (= (position-line start-pos) (position-line end-pos))
             (= (position-line start-pos) actual_line))
        ((lambda (res)
           (if (token? res)
               (token-STRING
                ; from docs on bytes->string/latin-1:
                ; "Produces a string by decoding the start to end substring of
                ; bstr as a Latin-1 encoding of Unicode code points; i.e., each
                ; byte is translated directly to a character using
                ; integer->char, so the decoding always succeeds. "
                (string-append (bytes->string/latin-1
                                ; we interpret the lexeme as a byte
                                (bytes (string->number
                                        (substring lexeme
                                                   1
                                                   (string-length lexeme)
                                                   ))))
                               (token-value res)))
               res
               )) ((lua-strings closing_char actual_line) input-port))
        ; {not (and (= (position-line start-pos) (position-line end-pos))
        ;           (= (position-line start-pos) actual_line))}
        (error "Lexer error. Lexeme:" lexeme)
        )]

   ; byte specified with hexadecimal numbers
   [byte-hexadecimal

    (if (and (= (position-line start-pos) (position-line end-pos))
             (= (position-line start-pos) actual_line))
        ((lambda (res)
           (if (token? res)
               (token-STRING
                (string-append (bytes->string/latin-1
                                ; we interpret the lexeme as a byte
                                (bytes (string->number
                                        ; we convert it into an hexa in racket
                                        (string-append "#x"
                                                       (substring
                                                        lexeme
                                                        2
                                                        (string-length lexeme)
                                                        )))))
                               (token-value res)))
               res)) ((lua-strings closing_char actual_line) input-port))
        ; {not (and (= (position-line start-pos) (position-line end-pos))
        ;           (= (position-line start-pos) actual_line))}
        (error "Lexer error. Lexeme:" lexeme)
        )]

   ; from ref. manual: "The escape sequence '\z' skips the following span of
   ; white-space characters, including line breaks;"
   [(re-: "\\z"
          (re-* whitespace))
    
    ; we just discard the lexeme; actual line may have changed
    ((lua-strings closing_char (position-line end-pos)) input-port)]
   
   ; ending
   [string-delimiter
    
    (if (equal? closing_char lexeme)
        ; ending
        (token-STRING "")
        ; lexeme must be appended to the rest of the string
        ((lambda (res)
           (if (token? res)
               (token-STRING
                (string-append lexeme
                               (token-value res)))
               res
               )) ((lua-strings closing_char actual_line) input-port)))]

   ; ref. manual: " A backslash followed by a real newline results in a newline
   ; in the string"
   ; the first backslash will be escaped, the real new line's backslash won't
   ["\\\n"
    
    ((lambda (res)
       (if (token? res)
           
           (token-STRING (string-append "\n" (token-value res)))
               
           res))
     ; actual line may have changed 
     ((lua-strings closing_char (position-line end-pos)) input-port))]

   ; any char
   [any-char

    (if (and (= (position-line start-pos) (position-line end-pos))
             (= (position-line start-pos) actual_line))
        ((lambda (res)
           (if (token? res)
               (token-STRING
                (string-append lexeme
                               (token-value res)))
               res
               )) ((lua-strings closing_char actual_line) input-port))
        (error "Lexer error. Lexeme:" lexeme))]))

; flags the beginning of a possible closing bracket
(define cons-long-bracket-end #f)
; the possible closing bracket
(define long-bracket-end "")
; strings in long brackets; receives the quantity eq_quant of = that
; the closing brackets must contain;
; for simplicity, the function ends returning a token-STRING or an error, without
; passing explicit control to lua-lexer
(define (lua-long-bracket-str eq_quant)
  (lexer
   ; consume as much chars as possible, before closing long brackets
   [not-long-bracket-str-end
    (begin
      (set! cons-long-bracket-end #f)
      (define aux long-bracket-end)
      (set! long-bracket-end "")
      (define res ((lua-long-bracket-str eq_quant) input-port))
      (if (token? res)
          ; res is a token: append previous closing long bracket candidate and
          ; lexeme, to its value
          (token-STRING
           ; TODO: "Any kind of end-of-line sequence (carriage return,
           ; newline, carriage return followed by newline, or newline followed
           ; by carriage return) is converted to a simple newline"
           (clean (string-append aux lexeme (token-value res))))
          ; res is an error
          res)
      )]

   ["]"
    (if cons-long-bracket-end
        ; we are consuming a closing long bracket
        (if (= (- (string-length long-bracket-end) 1) eq_quant)
            ; we found the end of the string: we return the empty string to handle
            ; the case of an empty string: [[]]
            (begin
              (token-STRING ""))
            ; not the expected long bracket: we need to append it to the result
            (begin
              ; we continue searching the correct ending long-bracket
              ((lambda (long-bracket-end-old dummy res)
                 (if (token? res)
                     ; res is a token: append long-bracket-end to its value
                     (token-STRING
                       (clean (string-append long-bracket-end-old
                                             (token-value res))))
                   ; res is an error
                     res)) long-bracket-end
                           (set! long-bracket-end "]")
                           ((lua-long-bracket-str eq_quant) input-port))))
        ; {not cons-long-bracket-end}
        ; first closing bracket found
        (begin
          ; set flags, save input and continue
          (set! cons-long-bracket-end #t)
          (set! long-bracket-end "]")
          ((lua-long-bracket-str eq_quant) input-port)))]

   ["="
    (if cons-long-bracket-end
        ; we are consuming a closing long bracket
        (begin
          (set! long-bracket-end (string-append long-bracket-end "="))
          ((lua-long-bracket-str eq_quant) input-port))
        ; {not cons-long-bracket-end}
        ; just append =
        (begin
          ((lambda (res)
             (if (token? res)
                 ; res is a token: append = to its value
                 (token-STRING
                   (clean (string-append "="
                                         (token-value res))))
                 ; res is an error
                 res)) ((lua-long-bracket-str eq_quant) input-port))))]
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
  ; Unescape backslashs from \r
  (set! aux (string-replace aux "\\r" "\r"))
  (set! aux (string-replace aux "\\\r" "\r"))
  ; Unescape backslashs from \t
  (set! aux (string-replace aux "\\t" "\t"))
  (set! aux (string-replace aux "\\\t" "\t"))

  (set! aux (string-replace aux "\\0" "\0"))
  (set! aux (string-replace aux "\\\0" "\\0"))
    
  (if (equal? aux string)
      string ; Nothing left to be cleaned
      (clean aux))
  )


(provide lua-lexer)