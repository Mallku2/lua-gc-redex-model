#lang racket
(require redex
         "../grammar.rkt"
         "./deltaBasic.rkt"
         "./deltaMath.rkt"
         "./deltaString.rkt"
         "./deltaTable.rkt")


; We define the semantics of the binary and unary operators of our language
; in terms of operations of PLT Racket. The "," symbol is treated as an escape
; to PLT Racket code. So, in general, the semantics of an expression
; (◇ op_1 op_2) is defined as the PLT Racket code (◇ (term op_1) (term op_2))
; when ◇ is also an operator of PLT Racket.
(define-metafunction ext-lang
  [(δ binop any ...)
   (δbasic binop any ...)]

  [(δ unop any ...)
   (δbasic unop any ...)]

  ; basic functions from the standard library
  [(δ builtinserv any ...)
   (δbasic builtinserv any ...)

   (side-condition (member (term builtinserv)
                           (term (assert
                                  collectgarbage
                                  error
                                  getmetatable
                                  ipairs
                                  load
                                  loadfile
                                  next
                                  pairs
                                  pcall
                                  print
                                  rawequal
                                  rawget
                                  rawlen
                                  rawset
                                  select
                                  setmetatable
                                  tonumber
                                  tostring
                                  type
                                  xpcall))))]

  ; math library
  [(δ builtinserv any ...)
   (δmath builtinserv any ...)

   (side-condition (member (term builtinserv)
                           (term (math.abs
                                  math.acos
                                  math.asin
                                  math.atan
                                  math.ceil
                                  math.cos
                                  math.cosh
                                  math.deg
                                  math.exp
                                  math.floor
                                  math.fmod
                                  math.log
                                  math.max
                                  math.modf
                                  math.rad
                                  math.sin
                                  math.sinh
                                  math.sqrt
                                  math.tan
                                  math.tanh))))]

  ; string library
  [(δ builtinserv any ...)
   (δstring builtinserv any ...)

   (side-condition (member (term builtinserv)
                           (term (string.dump
                                  string.len
                                  string.rep
                                  string.reverse
                                  string.sub))))]

  ; table library
  [(δ builtinserv any ...)
   (δtable builtinserv any ...)

   (side-condition (member (term builtinserv)
                           (term (table.concat
                                  table.insert
                                  table.pack
                                  table.unpack))))]
  
  
  ;                                                          
  ;                           ;                              
  ;                           ;                              
  ;                           ;                              
  ;                           ;                              
  ;   ;;;;;     ;;;     ;;;   ;   ;     ;;;    ;;;;;   ;;;;  
  ;   ;;  ;;   ;   ;   ;   ;  ;  ;     ;   ;  ;;  ;;  ;;  ;; 
  ;   ;    ;       ;  ;       ; ;          ;  ;    ;  ;    ; 
  ;   ;    ;   ;;;;;  ;       ;;;      ;;;;;  ;    ;  ;;;;;; 
  ;   ;    ;  ;    ;  ;       ;  ;    ;    ;  ;    ;  ;      
  ;   ;;  ;;  ;   ;;   ;   ;  ;   ;   ;   ;;  ;;  ;;  ;;   ; 
  ;   ;;;;;    ;;; ;    ;;;   ;    ;   ;;; ;   ;;; ;   ;;;;  
  ;   ;                                            ;         
  ;   ;                                        ;   ;         
  ;   ;                                         ;;;          
  
  
  ;                                                          
  ;                                     ;                    
  ;                                                          
  ;                                                          
  ;                                                          
  ;    ;;;;    ;;;;    ;;;;;  ;    ;  ;;;      ;;;;    ;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;;  ;    ;    ;      ;;  ;  ;;  ;; 
  ;    ;      ;    ;  ;    ;  ;    ;    ;      ;      ;    ; 
  ;    ;      ;;;;;;  ;    ;  ;    ;    ;      ;      ;;;;;; 
  ;    ;      ;       ;    ;  ;    ;    ;      ;      ;      
  ;    ;      ;;   ;  ;;  ;;  ;   ;;    ;      ;      ;;   ; 
  ;    ;       ;;;;    ;;; ;   ;;; ;  ;;;;;    ;       ;;;;  
  ;                        ;                                 
  ;                        ;                                 
  ;                        ;
  [(δ require θ)
   (δbasic error "bad argument #1 to 'require' (string expected, got no value)")]
  
  ; basic implementation of the require service
  [(δ require String_1 θ)
   (δbasic load String_2 nil nil nil θ)

   (where String_2
          ,(with-handlers ([exn:fail?
                            (λ (e) #f)])
             ((λ ()
                (file->string (term String_1))))))]
  
  ; something went wrong when trying to open file String_1
  [(δ require v_1 v_2 ... θ)
   (δbasic error "module not found")]
)

; To export the delta function
(provide δ)


;                                                                                          
;                                                           ;       ;;;                    
;                                                           ;         ;                    
;                     ;                       ;             ;         ;                    
;                     ;                       ;             ;         ;                    
;   ;;;;;;;  ;;;;   ;;;;;;    ;;;           ;;;;;;    ;;;   ;;;;;     ;      ;;;;    ;;;;  
;   ;  ;  ; ;;  ;;    ;      ;   ;            ;      ;   ;  ;;  ;;    ;     ;;  ;;  ;    ; 
;   ;  ;  ; ;    ;    ;          ;            ;          ;  ;    ;    ;     ;    ;  ;      
;   ;  ;  ; ;;;;;;    ;      ;;;;;   ;;;      ;      ;;;;;  ;    ;    ;     ;;;;;;   ;;;;  
;   ;  ;  ; ;         ;     ;    ;            ;     ;    ;  ;    ;    ;     ;            ; 
;   ;  ;  ; ;;   ;    ;     ;   ;;            ;     ;   ;;  ;;  ;;    ;     ;;   ;  ;    ; 
;   ;  ;  ;  ;;;;      ;;;   ;;; ;             ;;;   ;;; ;  ;;;;;      ;;;   ;;;;    ;;;;  
;                                                                                          
;                                                                                          
;                                                                                          

; since theses defs. make use of δ, and δ uses them too, they cannot be put in
; an external module

; chooses a handler for a binary operation
; PRE : {v_1, v_2 are the operands and String is the string that serves as
;       key to index the meta-table
; ret = (getBinHandler v_1 v_2 String θ)
; POS : {returns the value of v_1's meta-table indexed with key String (if
;        it belongs to the meta-table and the value is not nil or false) or
;        it returns the value of v_2's meta-table indexed with key String}
(define-metafunction ext-lang
  getBinHandler : v v String θ -> v
  
  [(getBinHandler v_1 v_2 String θ)
   any
   
   ; Determine if v_1 has meta-table
   (where any (indexMetaTable v_1 String θ))

   (side-condition (not (is_false_cond? (term any))))
   ]
  
  [(getBinHandler v_1 v_2 String θ)
   any_2
   
   ; Determine if v_1 has meta-table
   (where any (indexMetaTable v_1 String θ))

   ; no value associated with String
   (side-condition (is_false_cond? (term any)))
   
   (where any_2 (indexMetaTable v_2 String θ))
   
   (side-condition (not (is_false_cond? (term any_2))))]
  
  ; Otherwise...
  [(getBinHandler v_1 v_2 String θ)
   nil])

(provide getBinHandler)

; Chooses a handler for an unary operation
; PRE : {sv is the operand and String is the string that serves as
;       key to index the meta-table
; ret = (getUnaryHandler sv String θ)
; POS : {returns the value of sv's meta-table indexed with key String (if
;        it belongs to the meta-table and the value is not nil or false) or
;        nil}
(define-metafunction ext-lang
  getUnaryHandler : v String θ -> v
  
  [(getUnaryHandler v String θ)
   any
   ; Determine if sv has meta-table
   (where any (indexMetaTable v String θ))
   (side-condition (not (is_false_cond? (term any))))]
  
  ; Otherwise...
  [(getUnaryHandler v String θ)
   nil])

(provide getUnaryHandler)

; Obtain a handler for an equality comparison, following the criterion defined
; in the procedure of the same name, in Lua's reference manual
(define-metafunction ext-lang 
  
  ; The values compared are tables, with the same handler for the equality
  ; comparison
  [(getEqualHandler v_1 v_2 θ)
   any_1
   
   (side-condition (equal? (term (δ type v_1))
                           (term (δ type v_2))))
   
   (side-condition (equal? (term (δ type v_1))
                           "table"))
   
   (where any_1 (indexMetaTable v_1 "__eq" θ))
   
   (where any_2 (indexMetaTable v_2 "__eq" θ))

   ; compare handlers through Lua's equality
   (side-condition (equal? (term (δ == any_1 any_2))
                           (term true)))]
  
  ; the values compared are tables, with the different handlers for the equality
  ; comparison
  [(getEqualHandler v_1 v_2 θ)
   nil
   
   (side-condition (equal? (term (δ type v_1))
                           (term (δ type v_2))))
   
   (side-condition (equal? (term (δ type v_1))
                           "table"))]
  
  ; The types of the values compared are different, or they are not tables
  [(getEqualHandler v_1 v_2 θ)
   nil])

(provide getEqualHandler)

(define-metafunction ext-lang
  
  [(errmessage ArithWrongOps String_1 String_2)
   ,(string-append "attempt to perform arithmetic on a "
                   (term String_1)
                   " value.")
   
   (side-condition (not (equal? (term String_1)
                                "number")))]
  
  [(errmessage ArithWrongOps "number" String_2)
   ,(string-append "attempt to perform arithmetic on a "
                   (term String_2)
                   " value.")]
  
  [(errmessage StrConcatWrongOps "string" String_2)
   ,(string-append "attempt to concatenate a "
                   (term String_2)
                   " value.")]
  
  [(errmessage StrConcatWrongOps String_1 String_2)
   ,(string-append "attempt to concatenate a "
                   (term String_1)
                   " value.")]
  
  [(errmessage OrdCompWrongOps String_1 String_2)
   ,(string-append "attempt to compare "
                   (term String_1)
                   " with "
                   (term String_2))]
  
  [(errmessage NegWrongOp String)
   ,(string-append "attempt to perform arithmetic on a "
                   (term String)
                   " value.")]
  
  [(errmessage StrLenWrongOp String)
   ,(string-append "attempt to get length of a "
                   (term String)
                   " value.")]
  
  [(errmessage WFunCall String)
   ,(string-append "attempt to call a "
                   (term String)
                   " value.")]
  
  [(errmessage NonTable String)
   ,(string-append "attempt to index a "
                   (term String)
                   " value.")]
  )

(provide errmessage)

(define-metafunction ext-lang
  
  [(binopeventkey +)
   "__add"]
  
  [(binopeventkey -)
   "__sub"]
  
  [(binopeventkey *)
   "__mul"]
  
  [(binopeventkey /)
   "__div"]
  
  [(binopeventkey %)
   "__mod"]
  
  [(binopeventkey ^)
   "__pow"]
  
  [(binopeventkey ..)
   "__concat"]
  
  [(binopeventkey <)
   "__lt"]
  
  [(binopeventkey <=)
   "__le"])

(provide binopeventkey)

(define-metafunction ext-lang
  
  [(unopeventkey \#)
   "__len"]
  
  [(unopeventkey -)
   "__unm"])

(provide unopeventkey)

(define-metafunction ext-lang
  
  [(eventkey (($statFunCall ... v_1 (v_2 ...)) WFunCall))
   "__call"]

  [(eventkey ((v_1 \[ v_2 \]) explabel))
   "__index"]

  [(eventkey (((v_1 \[ v_2 \]) = v_3) statlabel))
   "__newindex"]
)
(provide eventkey)

