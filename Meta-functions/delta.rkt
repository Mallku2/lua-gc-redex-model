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
  ; __metatable field already defined in tid_1
  [(δ debug.setmetatable v_1 tid_1 θ_1)
   (θ_2 v_1)

   (where (osp_1 ...
           (tid_1 ((\{ field_1  ...
                       (\[ "__metatable" \] = v_2)
                       field_2  ... \}) v_3 pos))
           osp_2 ...) θ_1)

   (where tid_2 (getMetaTableRef v_1))

   ; meta-table already present
   (where (osp_3 ...
           (tid_2 intreptable_2)
           osp_4 ...) θ_1)

   ; set new meta-table
   (where θ_2 (osp_3 ...
               (tid_2 ((\{ field_1  ...
                           (\[ "__metatable" \] = v_2)
                           field_2 ... \}) v_3 pos))
               osp_4 ...))]

  ; __metatable field not defined in tid_1
  [(δ debug.setmetatable v_1 tid_1 θ_1)
   (θ_2 v_1)

   (where (osp_1 ...
           (tid_1 ((\{ field  ... \}) v_2 pos))
           osp_2 ...) θ_1)

   (where tid_2 (getMetaTableRef v_1))

   ; meta-table already present
   (where (osp_3 ...
           (tid_2 intreptable_2)
           osp_4 ...) θ_1)

   ; set new meta-table
   ; add "__metatable" field, and make it contain tid_1 (so a call to
   ; getmetatable returns the expected result)
   (where θ_2 (osp_3 ...
               (tid_2 ((\{ field  ... (\[ "__metatable" \] = tid_1) \}) v_2 pos))
               osp_4 ...))]

  ; set new meta-table
  ; __metatable field already defined in tid_1
  [(δ debug.setmetatable v_1 tid_1 θ_1)
   (θ_2 v_1)

   (where (osp_1 ...
           (tid_1 ((\{ field_1 ...
                       (\[ "__metatable" \] = v_2)
                       field_2 ... \}) v_3 pos))
           osp_2 ...) θ_1)

   (where tid_2 (getMetaTableRef v_1))

   ; set new meta-table
   (where θ_2 ((tid_2 ((\{ field_1 ...
                           (\[ "__metatable" \] = v_2)
                           field_2 ... \}) v_3 pos))
               osp_1 ...
               (tid_1 ((\{ field_1 ...
                       (\[ "__metatable" \] = v_2)
                       field_2 ... \}) v_3 pos))
               osp_2 ...))]

  [(δ debug.setmetatable v_1 tid_1 θ_1)
   (θ_2 v_1)

   (where (osp_1 ...
           (tid_1 ((\{ field ... \}) v_2 pos))
           osp_2 ...) θ_1)

   (where tid_2 (getMetaTableRef v_1))

   ; set new meta-table
   (where θ_2 ((tid_2 ((\{ field ...
                            ; add "__metatable" field, and make it contain
                            ; tid_1 (so a call to getmetatable returns the
                            ; expected result)
                           (\[ "__metatable" \] = tid_1) \}) v_2 pos))
               osp_1 ...
               (tid_1 ((\{ field ... \}) v_2 pos))
               osp_2 ...))]

  ; delete meta-table
  [(δ debug.setmetatable v nil θ_1)
   (θ_2 v)

   (where tid (getMetaTableRef v))

   ; meta-table already present
   ; TODO: this breaks soundness: if there is a reference to tid somewhere else,
   ; we end up with dandgling references
   (where (osp_1 ...
           (tid intreptable)
           osp_2 ...) θ_1)

   (where θ_2 (osp_1 ...
               osp_2 ...))]

  ; meta-table not set
  [(δ debug.setmetatable v nil θ)
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
