#lang racket
(require redex
         "../grammar.rkt"
         "./grammarMetaFunctions.rkt"
         "./deltaBasic.rkt")

(define-metafunction ext-lang
  ;                                                  
  ;                             ;                    
  ;                                                  
  ;             ;                                    
  ;             ;                                    
  ;    ;;;;   ;;;;;;   ;;;;   ;;;     ; ;;;    ;;;;; 
  ;   ;    ;    ;      ;;  ;    ;     ;;   ;  ;;  ;; 
  ;   ;         ;      ;        ;     ;    ;  ;    ; 
  ;    ;;;;     ;      ;        ;     ;    ;  ;    ; 
  ;        ;    ;      ;        ;     ;    ;  ;    ; 
  ;   ;    ;    ;      ;        ;     ;    ;  ;;  ;; 
  ;    ;;;;      ;;;   ;      ;;;;;   ;    ;   ;;; ; 
  ;                                                ; 
  ;                                            ;   ; 
  ;                                             ;;;  
  
  
  ;                                  
  ;        ;                         
  ;        ;                         
  ;        ;                         
  ;        ;                         
  ;    ;;;;;  ;    ;  ;;;;;;; ;;;;;  
  ;   ;;  ;;  ;    ;  ;  ;  ; ;;  ;; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;;  ;;  ;   ;;  ;  ;  ; ;;  ;; 
  ;    ;;;;;   ;;; ;  ;  ;  ; ;;;;;  
  ;                           ;      
  ;                           ;      
  ;                           ;

  ; PRE : {cid ∈ dom(θ)}
  [(δstring string.dump cid (osp_1 ... (cid functiondef) osp_2 ...))
   String
   
   (where String ,(str-flatten (term functiondef)))]

  [(δstring string.dump v θ)
   any
   
   (where any (δbasic error ,(string-append
                         "bad argument #1 (function expected, got "
                         (term (δbasic type v))
                         ")")))
   ]

  
  ;                          
  ;   ;;;                    
  ;     ;                    
  ;     ;                    
  ;     ;                    
  ;     ;      ;;;;   ; ;;;  
  ;     ;     ;;  ;;  ;;   ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;;;;;;  ;    ; 
  ;     ;     ;       ;    ; 
  ;     ;     ;;   ;  ;    ; 
  ;      ;;;   ;;;;   ;    ; 
  ;                          
  ;                          
  ;
  [(δstring string.len Number)
   (δstring string.len String)

   (where String (δbasic tostring Number ()))]
  
  [(δstring string.len String)
   (δbasic \# String)]
  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;    ;;;;    ;;;;   ;;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;; 
  ;    ;      ;    ;  ;    ; 
  ;    ;      ;;;;;;  ;    ; 
  ;    ;      ;       ;    ; 
  ;    ;      ;;   ;  ;;  ;; 
  ;    ;       ;;;;   ;;;;;  
  ;                   ;      
  ;                   ;      
  ;                   ;
  ; coercion
  [(δstring string.rep Number_1 Number_2 v)
   (δstring string.rep String Number_2 v)

   (where String (δbasic tostring Number_1 ()))]

  [(δstring string.rep String_1 Number_1 Number_2)
   (δstring string.rep String_1 Number_1 String_2)

   (where String_2 (δbasic tostring Number_2 ()))]

   ; negative number of reps.: returns empty string
  [(δstring string.rep String Number any)
   ""

   (side-condition (<= (term Number) 0))]

  ; {Number_1 > 0}
  ; Number_1 is not natural: the official interpreter takes floor(Number_2)
  [(δstring string.rep String Number_1 any)
   (δstring string.rep String Number_2 any)

   (where Number_2 ,(floor (term Number_1)))
   (side-condition (not (= (term Number_1) (term Number_2))))]
  
  [(δstring string.rep String 1 any)
   String]

  ; ref. manual: "The default value for sep is the empty string (that is, no
  ; separator). "
  [(δstring string.rep String Number nil)
   (δstring string.rep String Number "")]

  ; {v_2 != nil}
  [(δstring string.rep String_1 Number String_2)
   (δbasic .. any String_1)
   
   (where any ,(foldr (λ (str accum) (term (δbasic .. (δbasic .. String_1 String_2)
                                                  ,accum)))
                      (term (δbasic .. String_1 String_2))
                      (build-list (- (term Number) 2)
                                  (λ (nmbr) (term String_1)))))
   ]

  [(δstring string.rep v_1 v_2 v_3)
   (δbasic error "string.rep: arguments of the wrong type")]
  ;                                                          
  ;                                                          
  ;                                                          
  ;                                                          
  ;                                                          
  ;    ;;;;    ;;;;   ;    ;   ;;;;    ;;;;    ;;;;    ;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;;  ;;  ;;   ;;  ;  ;    ;  ;;  ;; 
  ;    ;      ;    ;   ;  ;   ;    ;   ;      ;       ;    ; 
  ;    ;      ;;;;;;   ;  ;   ;;;;;;   ;       ;;;;   ;;;;;; 
  ;    ;      ;        ;;;;   ;        ;           ;  ;      
  ;    ;      ;;   ;    ;;    ;;   ;   ;      ;    ;  ;;   ; 
  ;    ;       ;;;;     ;;     ;;;;    ;       ;;;;    ;;;;  
  ;                                                          
  ;                                                          
  ;
  [(δstring string.reverse Number)
   (δstring string.reverse String)

   (where String (δbasic tostring Number ()))]

  [(δstring string.reverse String)
   ,(list->string (reverse (string->list (term String))))]
  
  ;                          
  ;                   ;      
  ;                   ;      
  ;                   ;      
  ;                   ;      
  ;    ;;;;   ;    ;  ;;;;;  
  ;   ;    ;  ;    ;  ;;  ;; 
  ;   ;       ;    ;  ;    ; 
  ;    ;;;;   ;    ;  ;    ; 
  ;        ;  ;    ;  ;    ; 
  ;   ;    ;  ;   ;;  ;;  ;; 
  ;    ;;;;    ;;; ;  ;;;;;  
  ;                          
  ;
  ; coercion
  [(δstring string.sub Number_1 Number_2 ...)
   (δstring string.sub String Number_2 ...)

   (where String (δbasic tostring Number_1 ()))]

  ; correction of indices
  ; ref.manual: "If j is absent, then it is assumed to be equal to -1"
  [(δstring string.sub String Number)
   (δstring string.sub String Number -1)]
  
  ; Number_1 < 0
  [(δstring string.sub String Number_1 Number_2)
   (δstring string.sub String Number_3 Number_2)

   (side-condition (< (term Number_1) 0))

   ; Number_1 refers to a position in String, counting backwards from its
   ; last character
   (where Number_3 ,(add1 (+ (term (δbasic \# String))
                             (term Number_1))))]

  ; {Number_1 >= 0}
  ; if Number_2 is greater than # String, then, it is corrected to that length
  [(δstring string.sub String Number_1 Number_2)
   (δstring string.sub String Number_1 (δbasic \# String))

   (side-condition (< (term (δbasic \# String))
                      (exact-floor (term Number_2))))]

  ; {0 <= Number_1 ∧ Number_2 <= #String}
  ; Number_2 < 0
  [(δstring string.sub String Number_1 Number_2)
   (δstring string.sub String Number_1 Number_4)

   (where Number_3 (δbasic \# String))
   
   (side-condition (and (<= (* -1 (term Number_3)) (term Number_2))
                        (< (term Number_2) 0)))
   ; if j < 0 => it is not referring directly to the length of the substring to
   ; be extracted but, rather, to how much we should substract to the total
   ; length of the original string: #String + j + 1 (j < 0)
   (where Number_4 ,(add1 (+ (term Number_3)
                             (ceiling (term Number_2)))))]

  ; {0 <= Number_1 ∧ Number_2 <= #String}
  ; ref. man: "if, after the translation of negative indices, i is less than 1,
  ; it is corrected to 1"
  [(δstring string.sub String Number_1 Number_2)
   (δstring string.sub String 1 Number_2)

   (side-condition (< (term Number_1) 1))]

  ; {1 <= Number_1 ∧ Number_2 <= #String}
  ; if, after these corrections, Number_1 is greater than Number_2, the function
  ; returns the empty string. 
  [(δstring string.sub String Number_1 Number_2)
   ""

   (side-condition (< (term Number_2)
                      (term Number_1)))]

  ; normal case
  ; {1 <= Number_1 <= Number_2 <= # String}
  [(δstring string.sub String Number_1 Number_2)
   ; Lua's String represents immutable sequences of bytes: we convert a Racket
   ; string into a byte string, extract the expected sub-sequence of the bytes,
   ; and then back to string
   ,(bytes->string/utf-8 (string->bytes/utf-8 (term String)) #f
                         ; non-specified behavior: string.sub takes the floor of
                         ; its numeric parameters
                         (- (exact-floor (term Number_1)) 1)
                         (exact-floor (term Number_2)))]

  

  ; to capture the "no value" error for every builtinserv 
  [(δstring builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))]

  ; Services that don't modify theta
  [(δstring builtinserv v ... θ)
   (δbasic error any)

   (side-condition (member (term builtinserv)
                           (term (; string
                                  string.dump))))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]

  ; Services that modify theta
  [(δstring builtinserv v ... θ)
   (θ (δbasic error any))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ])
  
(provide δstring)