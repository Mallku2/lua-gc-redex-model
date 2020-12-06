#lang racket

(require redex
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
   0]

  [(reconstruct_number (Number_1 Number_2 ...) Number_3)
  ,(+ (* (term Number_1)
         (expt (term Number_3)
               (- (length (term (Number_1 Number_2 ...)))
                  1)))
      (term (reconstruct_number (Number_2 ...) Number_3)))]
  )

; converts String into a decimal number, trying to interpret String as a
; number in base Number; returns nil if the conversion fails
(define-metafunction ext-lang
  convert_string : String Number -> any

  ; negative number
  [(convert_string String_1 Number_1)
   ,(* (term Number_3) -1)

   ; it is a negative number
   (side-condition (equal? "-"
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

   ; it is a negative number
   (side-condition (not (equal? "-"
                                (substring (term String) 0 1))))
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

; for a given String, it returns a list of decimal numbers, each one
; corresponding to the decimal representation of each character of
; String, according to base Number
; PRE : {Number is positive}
(define-metafunction ext-lang
  number_to_digits : Number Number -> any

  [(number_to_digits Number _)
   (Number)
   
   (side-condition (< (term Number) 10))]

  ; {(> (term Number_1 ) 10)}
  [(number_to_digits Number_1 Number_2)
   (Number_5 ... Number_3)

   ; get last digit
   (where Number_3 ,(with-handlers ([exn:fail? (λ (e) (term nil))])
                      ((λ ()
                         (remainder (term Number_1) 10)))))
   ; check if digit belongs to base Number_2
   (side-condition (< (term Number_3) (term Number_2)))
   ; rest of the digits
   (where Number_4 ,(quotient (term Number_1) 10))
   ; get remaining digits
   (where (Number_5 ...) (number_to_digits Number_4 Number_2))
   ]

  ; default
  [(number_to_digits _ _)
   nil]
  )

; converts Number_1 into a decimal number, trying to interpret String as a
; number in base Number_2; returns nil if the conversion fails
(define-metafunction ext-lang
  convert_number : Number Number -> any

  [(convert_number Number_1 Number_2)
   Number_4

   (side-condition (> (term Number_1) 0))
   ; transform String into a list of decimal numbers, according to
   ; base Number_1
   (where (Number_3 ...) (number_to_digits Number_1 Number_2))
   ; reconstruct number in decimal form, according to base Number_1
   (where Number_4 (reconstruct_number (Number_3 ...) Number_2))]

  [(convert_number Number_1 Number_2)
   ,(* -1 (term Number_5))

   (side-condition (< (term Number_1) 0))
   ; change sign
   (where Number_3 ,(* -1 (term Number_1)))
   ; transform String into a list of decimal numbers, according to
   ; base Number_1
   (where (Number_4 ...) (number_to_digits Number_3 Number_2))
   ; reconstruct number in decimal form, according to base Number_1
   (where Number_5 (reconstruct_number (Number_4 ...) Number_2))]

  ; default: something went wrong
  [(convert_number _ _)
   nil]
  )

(provide convert_number)