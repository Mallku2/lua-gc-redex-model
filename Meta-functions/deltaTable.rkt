#lang racket
(require redex
         "../grammar.rkt"
         "./objStoreMetafunctions.rkt"
         "./valStoreMetafunctions.rkt"
         "./grammarMetafunctions.rkt"
         "./coercion.rkt"
         "./gc.rkt"
         "./deltaBasic.rkt"
         "../Desugar/parser.rkt"
         "../Desugar/lexer.rkt"
         "../Desugar/phrases_constructors.rkt")


; We define the semantics of the binary and unary operators of our language
; in terms of operations of PLT Racket. The "," symbol is treated as an escape
; to PLT Racket code. So, in general, the semantics of an expression
; (◇ op_1 op_2) is defined as the PLT Racket code (◇ (term op_1) (term op_2))
; when ◇ is also an operator of PLT Racket.
(define-metafunction ext-lang
  ;                                          
  ;                   ;       ;;;            
  ;                   ;         ;            
  ;     ;             ;         ;            
  ;     ;             ;         ;            
  ;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
  ;     ;      ;   ;  ;;  ;;    ;     ;;  ;; 
  ;     ;          ;  ;    ;    ;     ;    ; 
  ;     ;      ;;;;;  ;    ;    ;     ;;;;;; 
  ;     ;     ;    ;  ;    ;    ;     ;      
  ;     ;     ;   ;;  ;;  ;;    ;     ;;   ; 
  ;      ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
  ;                                          
  ;                                          
  ;                                          
  
  ;                                                  
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;     ;;;    ;;;;   ; ;;;     ;;;     ;;;   ;;;;;; 
  ;    ;   ;  ;;  ;;  ;;   ;   ;   ;   ;   ;    ;    
  ;   ;       ;    ;  ;    ;  ;            ;    ;    
  ;   ;       ;    ;  ;    ;  ;        ;;;;;    ;    
  ;   ;       ;    ;  ;    ;  ;       ;    ;    ;    
  ;    ;   ;  ;;  ;;  ;    ;   ;   ;  ;   ;;    ;    
  ;     ;;;    ;;;;   ;    ;    ;;;    ;;; ;     ;;; 
  ;                                                  
  ;                                                  
  ; Missing parameters
  [(δtable table.concat objref nil v_1 v_2 θ)
   (δtable table.concat objref "" v_1 v_2 θ)]

  [(δtable table.concat objref String nil v θ)
   (δtable table.concat objref String 1 v θ)]

  [(δtable table.concat objref_1 String v nil θ)
   (δtable table.concat objref_1
                    String
                    v
                    (δbasic \# evaluatedtable)
                    θ)

   (where ((objref_2 object_2) ...
           (objref_1 (evaluatedtable any_1 any_2))
           (objref_3 object_3) ...) θ)]
  
  ; simpler case
  [(δtable table.concat objref String Number_1 Number_2 θ)
   any_3

   ; quantity of fields to be accessed
   (where Number_3 ,(+ (- (term Number_2) (term Number_1))
                       1))

   (side-condition (> (term Number_3)
                      0))

   ; construct a list of indexing operations, with numeric indices from
   ; Number_1 to Number_2
   (where (any_1 any_2 ...)
          ,(build-list (inexact->exact (term Number_3))
                       (λ (nmbr) (term (objref \[ ,(+ nmbr (term Number_1)) \])))))

   ; apply string concatenation between each field, separated by String
   (where any_3 ,(foldl (λ (field accum) (term (,accum .. (String .. ,field))))
                        (term any_1)
                        (term (any_2 ...))))]

  ; default case
  [(δtable table.concat objref String Number_1 Number_2 θ)
   ""]

  ; wrong parameters
  [(δtable table.concat v_1 v_2 ... θ)
   (δbasic error "table.concat: table expected")

   (side-condition (not (is_tid? (term v_1))))]
  
  ;                                                  
  ;     ;                                            
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;   ;;;     ; ;;;    ;;;;    ;;;;    ;;;;   ;;;;;; 
  ;     ;     ;;   ;  ;    ;  ;;  ;;   ;;  ;    ;    
  ;     ;     ;    ;  ;       ;    ;   ;        ;    
  ;     ;     ;    ;   ;;;;   ;;;;;;   ;        ;    
  ;     ;     ;    ;       ;  ;        ;        ;    
  ;     ;     ;    ;  ;    ;  ;;   ;   ;        ;    
  ;   ;;;;;   ;    ;   ;;;;    ;;;;    ;         ;;; 
  ;                                                  
  ;                                                  
  ;
  [(δtable table.insert objref_1 nil v
                    ((objref_2 object_2) ...
                     (objref_1 ((\{ field ... \}) any ...))
                     (objref_3 object_3) ...))

   (((objref_2 object_2) ...
     (objref_1 ((\{ field ... (\[  Number \] = v) \}) any ...))
     (objref_3 object_3) ...) (< >))
   
   (where Number (δbasic \# (\{ field ... \})))]
  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;   ;;;;;     ;;;     ;;;   ;   ;  
  ;   ;;  ;;   ;   ;   ;   ;  ;  ;   
  ;   ;    ;       ;  ;       ; ;    
  ;   ;    ;   ;;;;;  ;       ;;;    
  ;   ;    ;  ;    ;  ;       ;  ;   
  ;   ;;  ;;  ;   ;;   ;   ;  ;   ;  
  ;   ;;;;;    ;;; ;    ;;;   ;    ; 
  ;   ;                              
  ;   ;                              
  ;   ;                              
  
  
  [(δtable table.pack v ...)
   any_3
   
   (where Number ,(length (term (v ...))))
   
   ; take the list of keys (naturals starting from 1),
   ; the list of values received, and construct
   ; table fields taking 2 elements, one from each list.
   (where any ,(map (λ (number value)
                      (append (term (\[ ))
                              (list number)
                              (term (\] = ))
                              (list value)))
                    ; Build the list of keys
                    (build-list (term Number) (λ (nmbr) (+ nmbr 1)))
                    ; and pass the values
                    (term (v ...))))
   
   ; Filter nil-valued fields
   (where any_2 ,(filter (λ (field)
                           (not (redex-match? ext-lang
                                              (\[ v \] = nil)
                                              field)))
                         (term any)))
   
   ; Add the parenthesis and the field "n"
   ; NOTE: it seems that the implementation counts even the nil-valued
   ; fields.
   (where any_3 ,(append (term (\{ ))
                         (term any_2)
                         (term ((\[ "n" \] = Number)))
                         (term ( \}))))]
  
  
  ;                                                  
  ;                                           ;      
  ;                                           ;      
  ;                                           ;      
  ;                                           ;      
  ;   ;    ;  ; ;;;   ;;;;;     ;;;     ;;;   ;   ;  
  ;   ;    ;  ;;   ;  ;;  ;;   ;   ;   ;   ;  ;  ;   
  ;   ;    ;  ;    ;  ;    ;       ;  ;       ; ;    
  ;   ;    ;  ;    ;  ;    ;   ;;;;;  ;       ;;;    
  ;   ;    ;  ;    ;  ;    ;  ;    ;  ;       ;  ;   
  ;   ;   ;;  ;    ;  ;;  ;;  ;   ;;   ;   ;  ;   ;  
  ;    ;;; ;  ;    ;  ;;;;;    ;;; ;    ;;;   ;    ; 
  ;                   ;                              
  ;                   ;                              
  ;                   ;
  [(δtable table.unpack objref_1 v_1 v_2 v_3 ... θ)
   any_2
   
   (where evaluatedtable (getTable objref_1 θ))

   ; set range of indexes. v_1 and v_2 should be nil or a number 
   (where Number_1 ,(if (not (equal? (term v_1)
                                     (term nil)))
                        (term v_1)
                        1) ; default first index
          )
   
   (where Number_2 ,(if (not (equal? (term v_2)
                                     (term nil)))
                        (term v_2)
                        (term (δbasic \# evaluatedtable)) ; Default last index
                        ))

   ; construct a tuple of table indexing expressions
   (where any_2 ,(append (term (< ))

                         (map (λ (index)
                                (append (term (objref_1 \[ ))
                                        (term (,index))
                                        (term (\]))))
                              
                              (range (exact-floor (term Number_1))
                                     (+ (exact-floor (term Number_2)) 1)))

                         (term ( >))))]

  ; erroneous cases
  [(δtable table.unpack v_1 v_2 ... θ)
   (δbasic error String_2)
   
   (where String_1 (δbasic type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append "bad argument #1 (table expected, got "
                                   (term String_1)
                                   ")"))]
  
  [(δtable table.unpack v_1 v_2 v_3 ... θ)
   (δbasic error String_2)
   
   (where String_1 (δbasic type v_2))
   
   (side-condition (not (equal? (term String_1)
                                "number")))
   
   (where String_2 ,(string-append "bad argument #2 (number expected, got "
                                   (term String_1)
                                   ")"))]
  
  ; Default case
  [(δtable table.unpack v_1 v_2 v_3 v_4 ... θ)
   (δbasic error String_2)
   
   (where String_1 (δbasic type v_3))
   
   (where String_2 ,(string-append "bad argument #3 (number expected, got "
                                   (term String_1)
                                   ")"))]

  ; to capture the "no value" error for every builtinserv 
  [(δtable builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))]

  ; Services that don't modify theta
  [(δtable builtinserv v ... θ)
   (δbasic error any)

   (side-condition (member (term builtinserv)
                           (term (; table
                                  table.concat
                                  table.unpack))))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]

  ; Services that modify theta
  [(δtable builtinserv v ... θ)
   (θ (δbasic error any))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]
  )
(provide δtable)