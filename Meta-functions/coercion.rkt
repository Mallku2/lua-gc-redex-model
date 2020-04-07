#lang racket

(require racket/format)
(require redex
         "../grammar.rkt")

; String to Number conversion.
; Mimics the behaviour of the procedure luaO_str2d defined in lobject.c of the Lua's
; source code.
(define-metafunction ext-lang
  ;toNumber : any -> any
  
  ; The string received has the character n or N.
  ; Reject 'inf' and 'nan'
  [(toNumber String Number)
   false
   (side-condition (regexp-match #rx"n|N" (term String)))]
  
  ; The string received has the character x or X.
  ; Hexadecimal numeric string?
  ; TODO: we are not using the base...
  [(toNumber String Number)
   (stringToHexaNumber String)
   (side-condition (regexp-match #rx"x|X" (term String)))]
  
  ; Converts a decimal numeric string to a number
  [(toNumber String Number)
   ; TODO: finish stringToNumber. We are, for the moment, using the implementation
   ; on Racket of the procedure
   any_2
   (where any ,(string->number (term String) (term Number)))
   (where any_2, (if (equal? (term any) #f)
                     (term false)
                     (term any)))]
  
  ; TODO: for the moment, we do not allow the conversion of a number to a different 
  ; base
  [(toNumber any Number)
   any]
  
  
  )

(provide toNumber)

; Mimics the behaviour of the procedure lua_strx2number defined in lobject.c of the
; Lua's source code
(define-metafunction ext-lang
  ; Correct representation of an hexa number
  [(stringToHexaNumber String)
   Number_8
   ; Eliminate white-spaces from the beginning or the end o String
   (where String_2 ,(string-trim (term String)))
   ; Determine the sign of the number
   (where Number (sign String_2))
   ; Eliminate minus sign, if present
   (where String_3 (eliminateSign String_2))
   ; String_3 must begins with "0x" or "0X".
   (side-condition (regexp-match-positions #rx"^0x|0X" (term String_3)))
   ; Skip the prefix for hexa numbers
   (where String_4 ,(substring (term String_3) 2 (string-length (term String_3))))
   ; Read as many hexadecimal digits it can
   (where String_5 (extractHexaDigits String_4))
   ; Extract the rest
   (where String_6 ,(substring (term String_4) (string-length (term String_5))
                               (string-length (term String_4))))                                                             
   ; Read the fractional part, if present
   (where String_7 (extractHexaFraction String_6))
   ; Extract the remaining part
   (where String_8 ,(substring (term String_6)
                               ; If String_6 begins with ".", take it into
                               ; account to determine where to begin the substring
                               (if (regexp-match #rx"^\\." (term String_6))
                                   (+ 1 (string-length (term String_7)))
                                   (string-length (term String_7)))
                               (string-length (term String_6))))
   ; We already have digits before or after the '.'
   (side-condition (or (not (equal? (string-length (term String_7)) 0))
                       (not (equal? (string-length (term String_5)) 0))))
   ; Each fractional digit divides value by 2^-4
   (where Number_1 ,(string-length (term String_7)))
   (where Number_2 ,(* (term Number_1) -4.0))
   ; Read exponent part, if present
   (where String_9 (extractHexaExponent String_8))
   ; The exponenet part, if present, is well-formed
   (side-condition (not (equal? (term String_9) (term error))))
   ; Determine the sign of the exponent
   (where Number_3 (sign String_9))
   ; Eliminate minus sign, if present
   (where String_10 (eliminateSign String_9))
   ; Construct the result
   ; Concatenate the digits before and after the '.'
   (where String_11 ,(string-append (term String_5) (term String_7)))
   ; Translate the resulting string to a decimal number
   (where Number_4 ,(* (term Number) (string->number (term String_11) 16)))
   ; Translate the exponent to a decimal number
   (where Number_5 ,(* (term Number_3) (if (equal? (term String_10) "")
                                           0
                                           (string->number (term String_10) 10))))
   (where Number_6 ,(+ (term Number_5) (term Number_2)))
   ; Compute the exponent of 2
   (where Number_7 ,(expt 2.0 (term Number_6)))
   ; The final result
   (where Number_8 ,(* (term Number_4) (term Number_7)))]
  
  ; If something gone wrong...
  [(stringToHexaNumber String)
   false])

; Determine if a string begins or not with "+"/"-". If that is the case,
; then it returns "-1", to indicate that it begins with "-", and it returns "1"
; if not
(define-metafunction ext-lang
  ; Begins with "-"
  [(sign String)
   -1
   (side-condition (regexp-match-positions #rx"^-" (term String)))]
  
  ; Begins with "+" or another character differnte thatn "-"
  [(sign String)
   1])

; Determine if a string begins or not with "+"/"-". If that is the case,
; then it eliminates the sign from the received string. If that is not the
; case, then it returns the received string
(define-metafunction ext-lang
  ; Begins with "-"
  [(eliminateSign String)
   String_2
   (side-condition (regexp-match-positions #rx"^-" (term String)))
   (where String_2 ,(substring (term String) 1 (string-length (term String))))]
  
  ; Begins with "+"
  [(eliminateSign String)
   String_2
   (side-condition (regexp-match-positions #rx"^\\+" (term String)))
   (where String_2 ,(substring (term String) 1 (string-length (term String))))]
  
  [(eliminateSign String)
   String])
  
; Extracts the fractional part of a string representation of an hexadecimal
; number.
; ret = (extractHexaFraction string)
; POS : {ret is a sequence of hexadecimal digits present in string, if string 
;        begins with '.' and already has hexadecimal digits or ret=="" otherwise}
(define-metafunction ext-lang
  
  [(extractHexaFraction String)
   (extractHexaDigits String_2)
   (side-condition (regexp-match #rx"^\\." (term String)))
   (where String_2 ,(substring (term String) 1 
                                 (string-length (term String))))]
  
  [(extractHexaFraction String)
   ""
   (side-condition (not (regexp-match #rx"^\\." (term String))))])

; Extracts the exponent part of a string representation of an hexadecimal
; number, in scientific notation.
; ret = (extractHexaExponent string)
; POS : {ret is a sequence of hexadecimal digits present in string (with minus 
;        sign, if present), if string begins with 'p' and already has hexadecimal
;        digits or ret=="" otherwise}
(define-metafunction ext-lang
  ;extractHexaExponent : String -> String
  
  ; The exponent is negative
  [(extractHexaExponent String)
   String_4
   (side-condition (regexp-match #rx"^p|^P" (term String)))
   (where String_2 ,(substring (term String) 1 (string-length (term String))))
   ; String begins with "p-"
   (side-condition (regexp-match #rx"^-" (term String_2)))
   (where String_3 ,(substring (term String_2) 1 (string-length (term String_2))))
   (where String_4 ,(string-append "-" (term (extractDecimalDigits String_3))))
   (side-condition (equal? (string-length (term String))
                           (+ (string-length (term String_4))
                              1)))]
  
  ; The exponent is positive, indicated with "+"
  [(extractHexaExponent String)
   String_4
   
   (side-condition (regexp-match #rx"^p|^P" (term String)))
   (where String_2 ,(substring (term String) 1 (string-length (term String))))
   ; String begins with "p+"
   (side-condition (regexp-match #rx"^\\+" (term String_2)))
   (where String_3 ,(substring (term String_2) 1 (string-length (term String_2))))
   (where String_4 (extractDecimalDigits String_3))
   (side-condition (equal? (string-length (term String))
                           (+ (string-length (term String_4))
                              2)))]
  
  
  ; The exponent is positive
  [(extractHexaExponent String)
   String_3
   
   (side-condition (regexp-match #rx"^p|^P" (term String)))
   (where String_2 ,(substring (term String) 1 (string-length (term String))))
   (where String_3 (extractDecimalDigits String_2))
   (side-condition (equal? (string-length (term String))
                           (+ (string-length (term String_3))
                              1)))]
  
  ; There is a malformed exponent part
  [(extractHexaExponent String)
   error
   (side-condition (> (string-length (term String)) 0))]
  
  ; There is no exponent part
  [(extractHexaExponent String)
   ""])

; Read as many decimal digits it can, from the beginning of the string
; received
(define-metafunction  ext-lang
  extractDecimalDigits : String -> String
  
  [(extractDecimalDigits String)
   ,(string-append (substring (term String) 0 1)
            (term (extractDecimalDigits String_2)))
   ; The string begins with a decimal digit
   (side-condition (regexp-match 
                    #rx"^0|^1|^2|^3|^4|^5|^6|^7|^8|^9"
                    (term String)))
   (where String_2 ,(substring (term String) 1 (string-length (term String))))]
  
  [(extractDecimalDigits String)
   ""
   ; The string begins with an hexadecimal digit
   (side-condition (not (regexp-match 
                    #rx"^0|^1|^2|^3|^4|^5|^6|^7|^8|^9"
                    (term String))))])  

; Mimics the behaviour of the procedure strtod of the standard c library for
; linux
(define-metafunction ext-lang
  stringToNumber : String -> Number
  )

; Read as many hexadecimal digits it can, from the beginning of the string
; received
(define-metafunction  ext-lang
  extractHexaDigits : String -> String
  
  [(extractHexaDigits String)
   ,(string-append (substring (term String) 0 1)
            (term (extractHexaDigits String_2)))
   ; The string begins with an hexadecimal digit
   (side-condition (regexp-match 
                    #rx"^0|^1|^2|^3|^4|^5|^6|^7|^8|^9|^a|^A|^b|^B|^c|^C|^d|^D|^e|^E|^f|^F"
                    (term String)))
   (where String_2 ,(substring (term String) 1 (string-length (term String))))]
  
  [(extractHexaDigits String)
   ""
   ; The string begins with an hexadecimal digit
   (side-condition (not (regexp-match 
                    #rx"^0|^1|^2|^3|^4|^5|^6|^7|^8|^9|^a|^A|^b|^B|^c|^C|^d|^D|^e|^E|^f|^F"
                    (term String))))])

; Number to String conversion
; Mimics the behaviour of the GLIBC's sprintf procedure
; TODO: actually we are using the Racket's implementation
(define-metafunction ext-lang
  ; To implement the behaviour of Lua's coercion:
  ; 1.0 .. 1.0 = "11"
  [(toString Number)
   ,(~a (inexact->exact (term Number)))
   (side-condition (eq? (remainder (* (term Number) 10) 10)
                        0.0))]
  [(toString any)
   ; From racket/format
   ,(~a (term any))])

(provide toString)