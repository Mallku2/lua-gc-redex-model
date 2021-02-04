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
;                                               
;                                               
;         ;           ;                         
;         ;           ;                         
;         ;           ;                         
;     ;;; ;    ;;;    ; ;;;    ;     ;    ;;; ; 
;    ;   ;;   ;   ;   ;;   ;   ;     ;   ;   ;; 
;   ;     ;  ;     ;  ;     ;  ;     ;  ;     ; 
;   ;     ;  ;     ;  ;     ;  ;     ;  ;     ; 
;   ;     ;  ;;;;;;;  ;     ;  ;     ;  ;     ; 
;   ;     ;  ;        ;     ;  ;     ;  ;     ; 
;    ;   ;;   ;    ;  ;;   ;   ;;   ;;   ;   ;; 
;     ;;; ;    ;;;;   ; ;;;     ;;;; ;    ;;; ; 
;                                             ; 
;                                        ;   ;; 
;                                         ;;;;  
;                                               

  [(δ debug.setmetatable v_1 v_2 v_3 v_4 ... θ)
   (δ debug.setmetatable v_1 v_2 θ)]

  ; plain setmetatable
  [(δ debug.setmetatable tid v θ)
   (δ setmetatable tid v θ)]

  ; meta-table already present
  [(δ debug.setmetatable v tid_1 θ_1)
   (θ_2 v)

   (where (osp_1 ...
           (tid_1 intreptable_1)
           osp_2 ...) θ_1)

   (where tid_2 (getMetaTableRef v))

   ; meta-table already present
   (where (osp_3 ...
           (tid_2 intreptable_2)
           osp_4 ...) θ_1)

   ; set new meta-table
   (where θ_2 (osp_3 ...
               (tid_2 intreptable_1)
               osp_4 ...))]

  ; set new meta-table
  [(δ debug.setmetatable v tid_1 θ_1)
   (θ_2 v)

   (where (osp_1 ...
           (tid_1 intreptable_1)
           osp_2 ...) θ_1)

   (where tid_2 (getMetaTableRef v))

   ; set new meta-table
   (where θ_2 ((tid_2 intreptable_1)
               osp_1 ...
               (tid_1 intreptable_1)
               osp_2 ...))]

  ; delete meta-table
  [(δ debug.setmetatable v nil θ_1)
   (θ_2 v)

   (where tid (getMetaTableRef v))

   ; meta-table already present
   (where (osp_1 ...
           (tid intreptable_1)
           osp_2 ...) θ_1)

   (where θ_2 (osp_1 ...
               osp_2 ...))]

  ; meta-table not set
  [(δ debug.setmetatable v nil θ_1)
   (θ v)]
   
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
  ; basic implementation of the require service
  [(δ require String_1 θ)
   (δbasic load String_2 nil nil nil θ)

   (where String_2
          ,(with-handlers ([exn:fail?
                            (λ (e) #f)])
             ((λ ()
                (file->string (term String_1))))))]

  ; to capture the "no value" error for every builtinserv 
  [(δ builtinserv v ...)
   (δ error any)

   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))]
  
  ; services that don't modify theta
  [(δ builtinserv v ... θ)
   (δ error any)

   (side-condition (member (term builtinserv)
                           (term (require))))
   
   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))
   ]

  ; services that modify theta
  [(δ builtinserv v ... θ)
   (θ (δbasic error any))

   (side-condition (member (term builtinserv)
                           (term (debug.setmetatable))))
   
   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))
   ]
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

