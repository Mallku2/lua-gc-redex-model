#lang racket
(require redex
         "../grammar.rkt"
         "./tablesMetafunctions.rkt"
         "./objStoreMetafunctions.rkt"
         "./valStoreMetafunctions.rkt"
         "./grammarMetafunctions.rkt"
         "./coercion.rkt"
         "./gc.rkt"
         "../Desugar/parser.rkt"
         "../Desugar/lexer.rkt"
         "../Desugar/phrases_constructors.rkt")


; We define the semantics of the binary and unary operators of our language
; in terms of operations of PLT Racket. The "," symbol is treated as an escape
; to PLT Racket code. So, in general, the semantics of an expression
; (◇ op_1 op_2) is defined as the PLT Racket code (◇ (term op_1) (term op_2))
; when ◇ is also an operator of PLT Racket.
(define-metafunction ext-lang
  ; arithmetic operations
  ; coercion
  [(δ binop v_1 v_2)
   (δ binop Number_1 Number_2)
   
   (side-condition (and
                    ; the following condition triggers coercion
                    (or (is_string? (term v_1))
                        (is_string? (term v_2)))
                    
                    (term (isArithBinOp binop))))
   
   (where Number_1 (δ tonumber v_1 nil))
   (where Number_2 (δ tonumber v_2 nil))]
  
  ; from https://www.lua.org/manual/5.2/manual.html#2.1:
  ; "Number represents real (double-precision floating-point) numbers";
  ; we reuse racket's implementation of double-precision IEEE floating-point
  ; numbers, flonums 
  [(δ + Number_1 Number_2)
   ,(+ (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ - Number_1 Number_2)
   ,(- (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ * Number_1 Number_2)
   ,(* (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
 
  [(δ / Number_1 Number_2)
   ,(/ (term Number_3) (term Number_4))

   ; to guarantee ieee 754 behavior, we apply a step of conversion to flonums;
   ; hence, for example, a division (/ 1 0) results in +inf.0, as in Lua,
   ; instead of raising an exception
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ ^ Number_1 Number_2)
   ,(expt (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ % Number_1 Number_2)
   ;a - math.floor(a/b)*b
   (δ - Number_3 (δ * (δ math.floor (δ / Number_3 Number_4)) Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]

  [(δ - String)
   (δ - Number)
   
   (where Number (δ tonumber String nil))]

  [(δ - Number)
   ,(- (term Number))
   
   (where Number_2 ,(real->double-flonum (term Number)))]
  
  ; Number comparison
  [(δ < Number_1 Number_2)
   (toBool ,(< (term Number_1) (term Number_2)))]
  
  [(δ <= Number_1 Number_2)
   (toBool ,(<= (term Number_1) (term Number_2)))]
  
  ; string comparison
  [(δ < String_1 String_2)
   (toBool ,(string<? (term String_1) (term String_2)))]
  
  [(δ <= String_1 String_2)
   (toBool ,(string<=? (term String_1) (term String_2)))]
  
  ; string concatenation
  ; coercion
  [(δ .. Number v)
   (δ .. String v)

   ; perform coercion only if the second parameter is also a string or a number
   (side-condition (or (is_string? (term v))
                       (is_number? (term v))))
   
   (where String (δ tostring Number ()))]

  [(δ .. String_1 Number)
   (δ .. String_1 String_2)
   
   (where String_2 (δ tostring Number ()))]
  
  [(δ .. String_1 String_2)
   ,(string-append (term String_1) (term String_2))]
  
  ; string length
  ; Racket's bytes-string, to simulate what Lua's # operator does.
  [(δ \# String)
   ,(bytes-length (string->bytes/utf-8 (term String)))]
  
  ; Table length: just the max. numeric key
  [(δ \# (\{ field ... \}))
   any

   ; extract numeric keys
   (where (Number_1 Number_2 ...) 
          ,(map (λ (field)
                  ; Extract the expression associated with the binding...
                  (bind-exp
                   ; number 2...
                   (list-ref
                    ; of the list of match-bindings...
                    (match-bindings
                     ; which should be of length 1.
                     (list-ref
                      
                      (redex-match ext-lang
                                   (\[ Number \] = any)
                                   field) 
                      0)) 
                    1)))
                ; Obtain the fields of the form (\[ Number \] = any)
                (filter (λ (field)
                          (redex-match ext-lang
                                       (\[ Number \] = any)
                                       field))
                        (term (field ...)))))
   
   (where any ,(argmax (λ (number) number) (term (Number_1 Number_2 ...))))]
  
  ; Table doesn't have numeric keys
  [(δ \# evaluatedtable)
   0]
  
  ; equality comparison
  ; numbers needs special treatment
  [(δ == Number_1 Number_2)
   (toBool ,(= (term Number_1) (term Number_2)))]
  
  [(δ == v_1 v_2)
   (toBool ,(equal? (term v_1) (term v_2)))]
  
  ; logical connectives
  [(δ and v e)
   v
   (side-condition (is_false_cond? (term v)))]
  
  ; try: a,b = true and g(), with g being a function that returns 2 or more 
  ; values
  [(δ and v e) 
   (\( e \))
   (side-condition (not (is_false_cond? (term v))))]
  
  [(δ or v e)
   v
   (side-condition (not (is_false_cond? (term v))))]
  
  ; Try: a,b = false or g(), with g being a function that returns 2 or more 
  ; values
  [(δ or v e)
   (\( e \))
   (side-condition (is_false_cond? (term v)))]
  
  [(δ not v)
   true

   ; v = nil, false?
   (side-condition (is_false_cond? (term v)))]
  
  [(δ not v) 
   false]

   ; default case of binop and unop
  [(δ any v ...)
   (δ error String)

   (side-condition (or (redex-match ext-lang unop (term any))
                       (redex-match ext-lang binop (term any))))
                               
   
   (where String ,(string-append (symbol->string (term any))
                              ": erroneous parameters"))]
  
  ; built-in services
  
  
  ;                                                                                                                          
  ;                             ;                        ;;                                     ;                            
  ;                                                     ;                                                                    
  ;   ;;;;;;                                            ;                               ;                                    
  ;   ;     ;                                           ;                               ;                                    
  ;   ;     ;   ;;;    ;;;;   ;;;       ;;;           ;;;;;   ;    ;  ; ;;;     ;;;   ;;;;;;  ;;;      ;;;;   ; ;;;    ;;;;  
  ;   ;     ;  ;   ;  ;    ;    ;      ;   ;            ;     ;    ;  ;;   ;   ;   ;    ;       ;     ;;  ;;  ;;   ;  ;    ; 
  ;   ;;;;;;       ;  ;         ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;  ;      
  ;   ;     ;  ;;;;;   ;;;;     ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;   ;;;;  
  ;   ;     ; ;    ;       ;    ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;       ; 
  ;   ;     ; ;   ;;  ;    ;    ;      ;   ;            ;     ;   ;;  ;    ;   ;   ;    ;       ;     ;;  ;;  ;    ;  ;    ; 
  ;   ;;;;;;   ;;; ;   ;;;;   ;;;;;     ;;;             ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;    ;;;;   ;    ;   ;;;;  
  ;                                                                                                                          
  ;                                                                                                                          
  ;                                                                                                                          
  
  
  
  ;                                                  
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;     ;;;    ;;;;    ;;;;    ;;;;    ;;;;   ;;;;;; 
  ;    ;   ;  ;    ;  ;    ;  ;;  ;;   ;;  ;    ;    
  ;        ;  ;       ;       ;    ;   ;        ;    
  ;    ;;;;;   ;;;;    ;;;;   ;;;;;;   ;        ;    
  ;   ;    ;       ;       ;  ;        ;        ;    
  ;   ;   ;;  ;    ;  ;    ;  ;;   ;   ;        ;    
  ;    ;;; ;   ;;;;    ;;;;    ;;;;    ;         ;;; 
  ;                                                  
  ;                                                  
  ;                                                  
  
  ; Assert: condition evaluates to false or nil
  [(δ assert v_1 v_2 v_3 ...)
   (δ error any)
   
   (side-condition (is_false_cond? (term v_1)))
   
   (where any ,(if (equal? (term v_2) (term nil))
                   ; v_2 is nil. Return default error message.
                   (term "assertion failed!")
                   (term v_2)))]
  
  ; Assert: condition evaluates to true
  [(δ assert v ...)
   (< v ... >)]

  
  ;                                                                                                                  
  ;                   ;;;     ;;;                                                     ;                              
  ;                     ;       ;                                                     ;                              
  ;                     ;       ;                       ;                             ;                              
  ;                     ;       ;                       ;                             ;                              
  ;     ;;;    ;;;;     ;       ;      ;;;;     ;;;   ;;;;;;   ;;;;;    ;;;    ;;;;   ;;;;;     ;;;    ;;;;;   ;;;;  
  ;    ;   ;  ;;  ;;    ;       ;     ;;  ;;   ;   ;    ;     ;;  ;;   ;   ;   ;;  ;  ;;  ;;   ;   ;  ;;  ;;  ;;  ;; 
  ;   ;       ;    ;    ;       ;     ;    ;  ;         ;     ;    ;       ;   ;      ;    ;       ;  ;    ;  ;    ; 
  ;   ;       ;    ;    ;       ;     ;;;;;;  ;         ;     ;    ;   ;;;;;   ;      ;    ;   ;;;;;  ;    ;  ;;;;;; 
  ;   ;       ;    ;    ;       ;     ;       ;         ;     ;    ;  ;    ;   ;      ;    ;  ;    ;  ;    ;  ;      
  ;    ;   ;  ;;  ;;    ;       ;     ;;   ;   ;   ;    ;     ;;  ;;  ;   ;;   ;      ;;  ;;  ;   ;;  ;;  ;;  ;;   ; 
  ;     ;;;    ;;;;      ;;;     ;;;   ;;;;     ;;;      ;;;   ;;; ;   ;;; ;   ;      ;;;;;    ;;; ;   ;;; ;   ;;;;  
  ;                                                                ;                                       ;         
  ;                                                            ;   ;                                   ;   ;         
  ;                                                             ;;;                                     ;;;          
  [(δ collectgarbage σ_1 θ_1 s)
   (gcFinWeak s σ_1 θ_1)
   ]
  
  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;    ;;;;    ;;;;    ;;;;    ;;;;    ;;;;  
  ;   ;;  ;;   ;;  ;   ;;  ;  ;;  ;;   ;;  ; 
  ;   ;    ;   ;       ;      ;    ;   ;     
  ;   ;;;;;;   ;       ;      ;    ;   ;     
  ;   ;        ;       ;      ;    ;   ;     
  ;   ;;   ;   ;       ;      ;;  ;;   ;     
  ;    ;;;;    ;       ;       ;;;;    ;     
  ;                                          
  ;                                          
  ;                                          
  
  [(δ error v)
   ($err v)]
  
  ; TODO: model level
  [(δ error v_1 v_2)
   ($err v_1)]
  
  
  ;                                                                                                  
  ;                                                                           ;       ;;;            
  ;                                                                           ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;    ;;;;;   ;;;;   ;;;;;;  ;;;;;;;  ;;;;   ;;;;;;    ;;;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
  ;   ;;  ;;  ;;  ;;    ;     ;  ;  ; ;;  ;;    ;      ;   ;    ;      ;   ;  ;;  ;;    ;     ;;  ;; 
  ;   ;    ;  ;    ;    ;     ;  ;  ; ;    ;    ;          ;    ;          ;  ;    ;    ;     ;    ; 
  ;   ;    ;  ;;;;;;    ;     ;  ;  ; ;;;;;;    ;      ;;;;;    ;      ;;;;;  ;    ;    ;     ;;;;;; 
  ;   ;    ;  ;         ;     ;  ;  ; ;         ;     ;    ;    ;     ;    ;  ;    ;    ;     ;      
  ;   ;;  ;;  ;;   ;    ;     ;  ;  ; ;;   ;    ;     ;   ;;    ;     ;   ;;  ;;  ;;    ;     ;;   ; 
  ;    ;;; ;   ;;;;      ;;;  ;  ;  ;  ;;;;      ;;;   ;;; ;     ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
  ;        ;                                                                                         
  ;    ;   ;                                                                                         
  ;     ;;;                                                                                          
  ; Table value has a meta-table, which has a "__metatable" key.
  [(δ getmetatable objref_1 (osp_1 ...
                              (objref_1 (tableconstructor objref_3 any_1))
                              osp_2 ...))
   v
   
   (where (osp_3 ...
           (objref_3 ((\{ field_1 ... (\[ "__metatable" \] = v) field_2 ... \})
                      any_2 ...))
           osp_4 ...)

          (osp_1 ...
           (objref_1 (tableconstructor objref_3 any_1))
           osp_2 ...))

   (side-condition (not (equal? (term v)
                                (term nil))))
   ]

  ; Table value doesn't have a protected meta-table
  [(δ getmetatable objref (osp_1 ...
                            (objref (tableconstructor v any))
                            osp_2 ...))
   v]

  
  ; The value isn't a table. It has a meta-table, which has
  ; a "__metatable" key,
  [(δ getmetatable any_1 (osp_1 ...
                           (objref_2 ((\{ field ...
                                          (\[ "__metatable" \] = v)
                                          field ... \})
                                      any_2 ...))
                           osp_2 ...))
   v
   
   (where objref_2 (getMetaTableRef any_1))
   ]
  
  ; The value isn't a table. It has a meta-table,
  ; which has not a "__metatable" key
  [(δ getmetatable any_1 (osp_1 ...
                           (objref_2 any_2)
                           osp_2 ...))
   objref_2
   
   (where objref_2 (getMetaTableRef any_1))
   ]
  
  ; The value isn't a table. Its type has not a meta-table set.
  [(δ getmetatable any θ)
   nil]
  
  
  ;                                                  
  ;     ;                       ;                    
  ;                                                  
  ;                                                  
  ;                                                  
  ;   ;;;     ;;;;;     ;;;   ;;;      ;;;;    ;;;;  
  ;     ;     ;;  ;;   ;   ;    ;      ;;  ;  ;    ; 
  ;     ;     ;    ;       ;    ;      ;      ;      
  ;     ;     ;    ;   ;;;;;    ;      ;       ;;;;  
  ;     ;     ;    ;  ;    ;    ;      ;           ; 
  ;     ;     ;;  ;;  ;   ;;    ;      ;      ;    ; 
  ;   ;;;;;   ;;;;;    ;;; ;  ;;;;;    ;       ;;;;  
  ;           ;                                      
  ;           ;                                      
  ;           ;
  
  ; Custom iterator, provided by the metatable
  [(δ ipairs objref θ)
   ((function $IpairsCustomIter ()
              (local v1 v2 v3 = (any (objref))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (where any (indexMetaTable objref "__ipairs" θ))
   
   (side-condition (not (equal? (term any)
                                (term nil))))]

  ; Default iterator
  [(δ ipairs objref θ)
   (< (function $iPairsDefaultIter (t var)
                (local result ttype = nil ($builtIn type (t))
                  in
                  ((if (ttype == "table")
                       then
                       
                       \;
                       
                       else
                       
                       (return
                        ($builtIn
                         error
                         ((("bad argument #1 (table expected, got" .. ttype) .. ")"))))
                       
                       end)
                   
                   (var = (var + 1))
                   
                   (result = ($builtIn rawget (t var)))
                   
                   (if (result == nil)
                       then
                       
                       (return (< nil >))
                       
                       else
                       
                       (return (< var result >))
                       
                       end))
                  end)
                end) objref 0 >)
   
   (where nil (indexMetaTable objref "__ipairs" θ))]
  
  [(δ ipairs v ... θ)
   (δ error "bad argument #1 (table expected)")]
  
  
  ;                                  
  ;   ;;;                          ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;     ;      ;;;;     ;;;    ;;;;; 
  ;     ;     ;;  ;;   ;   ;  ;;  ;; 
  ;     ;     ;    ;       ;  ;    ; 
  ;     ;     ;    ;   ;;;;;  ;    ; 
  ;     ;     ;    ;  ;    ;  ;    ; 
  ;     ;     ;;  ;;  ;   ;;  ;;  ;; 
  ;      ;;;   ;;;;    ;;; ;   ;;;;; 
  ;                                  
  ;                                  
  ;
  [(δ load String v_1 "b" v_3)
   any_2

   ; String should be a string (flattened) representation of a function
   ; (obtained through string.dump). We reuse Racket's reader to
   ; try to parse it and obtain the corresponding redex term.
   (where any_1 ,(read (open-input-string (term String))))
   
   (where any_2 ,(if (is_fdef? (term any_1))

                     (term any_1)

                     (term (< nil
                              "attempt to load a text chunk (mode is 'b')"
                              >))))]
 
  ; Lua program expressed into a string, either syntactically correct or not 
  [(δ load String v_1 "t" v_3)
   any_2
   
   (where any_1 ,(with-handlers
                     ([exn:fail?
                       (λ (e) (append (term (< nil ))
                                      (list (if (equal? (term v_1) (term nil))
                                                
                                                (string-append "[string "
                                                               (term String)
                                                               "]")
                                                           
                                                           (term v_1)))
                                                 (term ( >))))])
                   
                   (parse-this (term String)
                               #t
                               ; TODO: the reference to _G is hardcoded here...
                               (term (to-abstract (ref 1)))
                               )
                   
                   ))
   
   (where any_2 ,(if (not (redex-match ext-lang
                                       (< e ... >)
                                       (term any_1)))
                     ; If the parsing succeeded, return the code obtained
                     ; wrapped into a vararg function.
                     (append (term (function $loaded (<<<)))
                             (if (not (equal? (term v_3) (term nil)))
                                 ; Received new value for the global
                                 ; environment
                                 (list (term
                                        (local $old_env $ret = (ref 1) nil
                                          in
                                          (((ref 1) = v_3)
                                           ($ret = ((\( (function $aux (<<<)
                                                                  any_1
                                                                  end) \))
                                                    (<<<)))
                                           ((ref 1) = $old_env)
                                           (return $ret))
                                          end)))
                                 
                                 (list (term any_1)))
                             
                             (term (end)))
                     ; Otherwise, return the error message. 
                     (term any_1)))]

  ; Default behavior for mode: "bt"
  [(δ load String v_1 v_2 v_3)
   any

   (where any (δ load String v_1 "b" v_3))

   (side-condition (is_fdef? (term any)))]

  [(δ load String v_1 v_2 v_3)
   (δ load String v_1 "t" v_3)
   ]
  
  ; Received a function from which we can obtain the string that represents the
  ; program
  [(δ load cid v_1 v_2 v_3)
   any_2
   
   ; Set the appropriate value to the "source" argument (second argument)
   (where any ,(if (equal? (term v_1) (term nil))
                   
                   "=(load)"
                   
                   (term v_1)))

   ; TODO: we don't impose a limit to the times the function is called:
   ; load(function() return "return true" end) is and endless loop in our
   ; mechanization, while it isn't the case for the Lua official interpreter.

   (where any_2 ((function $loaded ()
                           (local program nextPiece = "" ""
                             in
                             ((nextPiece = (cid ()))
                              
                              (while (not ((nextPiece == "")
                                           or
                                           (nextPiece == nil)))
                                     do
                                     
                                     (if (($builtIn type (nextPiece))
                                          ==
                                          "string")
                                         then
                                         ((program = (program .. nextPiece))
                                          (nextPiece = (cid ())))
                                         else
                                         (return (< nil
                                                    "reader function must
                                                     return a string" >))
                                         end)
                                     
                                     end)
                              
                              (return ($builtIn load (program any v_2 v_3))))
                             
                             end)
                           end) ()))
   ]

  ; Default case
  [(δ load v ...)
   (δ error String)
   
   (where String ,(string-append "load: bad argument #1"))]

  
  ;                                                                  
  ;   ;;;                          ;     ;;     ;     ;;;            
  ;     ;                          ;    ;               ;            
  ;     ;                          ;    ;               ;            
  ;     ;                          ;    ;               ;            
  ;     ;      ;;;;     ;;;    ;;;;;  ;;;;;   ;;;       ;      ;;;;  
  ;     ;     ;;  ;;   ;   ;  ;;  ;;    ;       ;       ;     ;;  ;; 
  ;     ;     ;    ;       ;  ;    ;    ;       ;       ;     ;    ; 
  ;     ;     ;    ;   ;;;;;  ;    ;    ;       ;       ;     ;;;;;; 
  ;     ;     ;    ;  ;    ;  ;    ;    ;       ;       ;     ;      
  ;     ;     ;;  ;;  ;   ;;  ;;  ;;    ;       ;       ;     ;;   ; 
  ;      ;;;   ;;;;    ;;; ;   ;;;;;    ;     ;;;;;      ;;;   ;;;;  
  ;                                                                  
  ;                                                                  
  ;                                                                  
  [(δ loadfile String_1 v_2 v_3 θ)
   (δ load String_2 nil v_2 v_3)

   (where String_2 ,(with-handlers
                     ([exn:fail? (λ (e) (term nil))])
                      (file->string (term String_1))
                   ))]

  ; no file found
  [(δ loadfile String_1 v_2 v_3 θ)
   nil

   (where nil ,(with-handlers
                   ([exn:fail? (λ (e) (term nil))])
                 (file->string (term String_1))
                 ))]

  [(δ loadfile v_1 v_2 v_3 θ)
   (δ error String_1)

   (where String_1 (δ type v_1))

   (where String_2 ,(string-append "bad argument #1 (string expected, got "
                                   (term String_1)
                                   ")"))]
  
  ;                                  
  ;                                  
  ;                                  
  ;                             ;    
  ;                             ;    
  ;   ; ;;;    ;;;;   ;;  ;;  ;;;;;; 
  ;   ;;   ;  ;;  ;;   ;  ;     ;    
  ;   ;    ;  ;    ;    ;;      ;    
  ;   ;    ;  ;;;;;;    ;;      ;    
  ;   ;    ;  ;         ;;      ;    
  ;   ;    ;  ;;   ;   ;  ;     ;    
  ;   ;    ;   ;;;;   ;;  ;;     ;;; 
  ;                                  
  ;                                  
  ;
  
  ; Bad argument #1
  [(δ next v_1 v_2 θ)
   (δ error String_2)
   
   (where String_1 (δ type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 to 'next' (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {the first argument is a pointer to a table}

  ; From the ref. manual: "The order in which the indices are enumerated is not
  ; specified, even for numeric indices".
  ; We use the order of occurrence of the fields in the int. rep. of the table 

  ; nil index, non empty table
  [(δ next objref nil (osp_1 ...
                        (objref ((\{ (\[ v_1 \] = v_2) field ... \}) any ...))
                        osp_2 ...))
   (< v_1 v_2 >)]
  
  ; nil index, empty table
  [(δ next objref nil θ)
   (< nil >)]
  
  ; Not the last index
  [(δ next objref v_1 (osp_1 ...
                        (objref
                         ((\{ field_1 ... (\[ v_2 \] = v_3) (\[ v_4 \] = v_5)
                              field_2 ... \})
                          any ...))
                        osp_2 ...))
   (< v_4 v_5 >)
   
   (side-condition (is_true? (term (δ == v_1 v_2))))
   ]
  
  ; Last index
  [(δ next objref v_1 (osp_1 ...
                        (objref
                         ((\{ field_1 ... (\[ v_2 \] = v_3) \}) any ...))
                        osp_2 ...))
   (< nil >)
   
   (side-condition (is_true? (term (δ == v_1 v_2))))
   ]
  
  ; Invalid key.
  [(δ next objref v θ)
   (δ error "invalid key to 'next'")]
  
  
  ;                                          
  ;                     ;                    
  ;                                          
  ;                                          
  ;                                          
  ;   ;;;;;     ;;;   ;;;      ;;;;    ;;;;  
  ;   ;;  ;;   ;   ;    ;      ;;  ;  ;    ; 
  ;   ;    ;       ;    ;      ;      ;      
  ;   ;    ;   ;;;;;    ;      ;       ;;;;  
  ;   ;    ;  ;    ;    ;      ;           ; 
  ;   ;;  ;;  ;   ;;    ;      ;      ;    ; 
  ;   ;;;;;    ;;; ;  ;;;;;    ;       ;;;;  
  ;   ;                                      
  ;   ;                                      
  ;   ;

  ; Custom iterator, provided by the metatable
  [(δ pairs objref θ)
   ((function $pairsCustomIter ()
              (local v1 v2 v3 = (any (objref))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (where any (indexMetaTable objref "__pairs" θ))
   
   (side-condition (not (is_nil? (term any))))]

  ; Default iterator: next
  [(δ pairs objref θ)
   (< (function $next (table index)
                (return ($builtIn next (table index)))
                end) objref nil >)
   
   (where nil (indexMetaTable objref "__pairs" θ))]
  
  [(δ pairs v θ)
   (δ error String_2)
   
   (where String_1 (δ type v))
   
   (where String_2 ,(string-append "bad argument #1 (table expected, got "
                                   (term String_1)
                                   ")"))]
  
  ;                                          
  ;                           ;;;     ;;;    
  ;                             ;       ;    
  ;                             ;       ;    
  ;                             ;       ;    
  ;   ;;;;;     ;;;     ;;;     ;       ;    
  ;   ;;  ;;   ;   ;   ;   ;    ;       ;    
  ;   ;    ;  ;            ;    ;       ;    
  ;   ;    ;  ;        ;;;;;    ;       ;    
  ;   ;    ;  ;       ;    ;    ;       ;    
  ;   ;;  ;;   ;   ;  ;   ;;    ;       ;    
  ;   ;;;;;     ;;;    ;;; ;     ;;;     ;;; 
  ;   ;                                      
  ;   ;                                      
  ;   ;                                      
  
  [(δ pcall v_1 v_2 ...)
   ; pcall is defined in terms of xpcall
   ($builtIn xpcall (v_1
                     (function $handler (errMsg) (return false errMsg) end)
                     v_2 ...))]

  
  ;                                          
  ;                     ;                    
  ;                                          
  ;                                     ;    
  ;                                     ;    
  ;   ;;;;;    ;;;;   ;;;     ; ;;;   ;;;;;; 
  ;   ;;  ;;   ;;  ;    ;     ;;   ;    ;    
  ;   ;    ;   ;        ;     ;    ;    ;    
  ;   ;    ;   ;        ;     ;    ;    ;    
  ;   ;    ;   ;        ;     ;    ;    ;    
  ;   ;;  ;;   ;        ;     ;    ;    ;    
  ;   ;;;;;    ;      ;;;;;   ;    ;     ;;; 
  ;   ;                                      
  ;   ;                                      
  ;   ;

  ; Hack to implement a simple instrumentation tool.
  [(δ print v_1 ... ((refStdout String_1) vsp ...) θ)
   ((refStdout String_3) vsp ...)

   ; concat v_1 ... into a single string
   (where String_2 ,(foldl (λ (str accum)
                             (string-append accum
                                            (term (δ tostring ,str θ))
                                            )
                            )
                           (term String_1)
                           (term (v_1 ...))))

   (where String_3, (string-append (term String_2)
                                   "\n"))
   ]

  ; nothing has been written to stdout
  [(δ print v_1 ... (vsp ...) θ)
   ((refStdout String_2) vsp ...)
   
   (where String_1 ,(foldl (λ (str accum)
                             (string-append accum
                                            (term (δ tostring ,str θ))
                                            )
                            )
                           (term " ")
                           (term (v_1 ...))))

   (where String_2 ,(string-append (term String_1)
                                   "\n"))
   ]
  
  ;                                                                  
  ;                                                           ;;;    
  ;                                                             ;    
  ;                                                             ;    
  ;                                                             ;    
  ;    ;;;;     ;;;  ;      ;  ;;;;    ;;;;;  ;    ;    ;;;     ;    
  ;    ;;  ;   ;   ; ;      ; ;;  ;;  ;;  ;;  ;    ;   ;   ;    ;    
  ;    ;           ;  ; ;; ;  ;    ;  ;    ;  ;    ;       ;    ;    
  ;    ;       ;;;;;  ; ;; ;  ;;;;;;  ;    ;  ;    ;   ;;;;;    ;    
  ;    ;      ;    ;  ; ;; ;  ;       ;    ;  ;    ;  ;    ;    ;    
  ;    ;      ;   ;;   ;  ;   ;;   ;  ;;  ;;  ;   ;;  ;   ;;    ;    
  ;    ;       ;;; ;   ;  ;    ;;;;    ;;; ;   ;;; ;   ;;; ;     ;;; 
  ;                                        ;                         
  ;                                        ;                         
  ;                                        ;                         
  
  [(δ rawequal v_1 v_2)
   (δ == v_1 v_2)]
  
  ;                                                  
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;    ;;;;     ;;;  ;      ;  ;;;;;   ;;;;   ;;;;;; 
  ;    ;;  ;   ;   ; ;      ; ;;  ;;  ;;  ;;    ;    
  ;    ;           ;  ; ;; ;  ;    ;  ;    ;    ;    
  ;    ;       ;;;;;  ; ;; ;  ;    ;  ;;;;;;    ;    
  ;    ;      ;    ;  ; ;; ;  ;    ;  ;         ;    
  ;    ;      ;   ;;   ;  ;   ;;  ;;  ;;   ;    ;    
  ;    ;       ;;; ;   ;  ;    ;;; ;   ;;;;      ;;; 
  ;                                ;                 
  ;                            ;   ;                 
  ;                             ;;;                  
  
  ; Bad argument #1
  [(δ rawget v_1 v_2 θ)
   (δ error String_2)
   
   (where String_1 (δ type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append 
                     "bad argument #1 (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {objref points to a table}
  [(δ rawget objref_1 v_1
              (osp_1 ...
               (objref_1 ((\{ field_1 ... (\[ v_2 \] = v_3) field_2 ... \})
                          any ...))
               osp_2 ...))
   v_3
   
   (side-condition (equal? (term (δ == v_1 v_2))
                           'true))]
  
  ; {v isn't a key of the table pointed by objref}
  [(δ rawget objref v θ)
   nil]
  
  
  ;                                                  
  ;                           ;;;                    
  ;                             ;                    
  ;                             ;                    
  ;                             ;                    
  ;    ;;;;     ;;;  ;      ;   ;      ;;;;   ; ;;;  
  ;    ;;  ;   ;   ; ;      ;   ;     ;;  ;;  ;;   ; 
  ;    ;           ;  ; ;; ;    ;     ;    ;  ;    ; 
  ;    ;       ;;;;;  ; ;; ;    ;     ;;;;;;  ;    ; 
  ;    ;      ;    ;  ; ;; ;    ;     ;       ;    ; 
  ;    ;      ;   ;;   ;  ;     ;     ;;   ;  ;    ; 
  ;    ;       ;;; ;   ;  ;      ;;;   ;;;;   ;    ; 
  ;                                                  
  ;                                                  
  ;                                                  
  ; Bad argument #1 to 'rawlen'
  [(δ rawlen v θ)
   (δ error "bad argument #1 to 'rawlen'(table or string expected)")
   
   (where string (δ type v))
   
   (side-condition (not (or (equal? (term string)
                                    "table")
                            
                            (equal? (term string)
                                    "string"))))]
  
  [(δ rawlen String θ)
   (δ \# String)]
  
  [(δ rawlen objref_1 (osp_1 ...
                        (objref_1 (tableconstructor any ...))
                        osp_2 ...))
   (δ \# tableconstructor)]
  
  ;                                                  
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;    ;;;;     ;;;  ;      ;  ;;;;    ;;;;   ;;;;;; 
  ;    ;;  ;   ;   ; ;      ; ;    ;  ;;  ;;    ;    
  ;    ;           ;  ; ;; ;  ;       ;    ;    ;    
  ;    ;       ;;;;;  ; ;; ;   ;;;;   ;;;;;;    ;    
  ;    ;      ;    ;  ; ;; ;       ;  ;         ;    
  ;    ;      ;   ;;   ;  ;   ;    ;  ;;   ;    ;    
  ;    ;       ;;; ;   ;  ;    ;;;;    ;;;;      ;;; 
  ;                                                  
  ;                                                  
  ;                                                  
  
  ; Bad argument #1 to 'rawset'
  [(δ rawset v_1 v_2 v_3 θ)
   (θ (δ error String_2))
   
   (where String_1 (δ type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 (table expected, got "
                     (term String_1)
                     ")"))]

  ; {(δ type v_1) == "table"}
  ; Bad argument #2 to 'rawset'
  [(δ rawset v_1 nil v_3 θ)
   (θ (δ error String_2))
   
   (where String_2 "table index is nil")]
  
  [(δ rawset v_1 +nan.0 v_3 θ)
   (θ (δ error String_2))
   
   (where String_2 "table index is NaN")]
  
  ; Delete field
  [(δ rawset tid v_1 nil θ)
   ((osp_1 ...
     (tid ((\{ field_1 ... field_2 ... \}) any ...))
     osp_2 ...) tid)

   (where (side-condition
           (tid v_1 (osp_1 ...
                     (tid ((\{ field_1 ... (\[ v_2 \] = v_3)
                               field_2 ... \})
                           any ...))
                     osp_2 ...))
           (equal? (term (δ == v_1 v_2))
                   'true))
          (tid v_1 θ))
   ]
  
  ; v_2 != nil
  [(δ rawset objref v_1 v_2 (osp_1 ...
                              (objref ((\{ field_1 ... (\[ v_3 \] = v_4)
                                           field_2
                                           ... \})
                                       any ...))
                              osp_2 ...))
   ((osp_1 ...
     (objref ((\{ field_1 ... (\[ v_3 \] = v_2) field_2 ... \}) any ...))
     osp_2 ...) objref)

   (side-condition (equal? (term (δ == v_1 v_3))
                           'true))
   ]
  
  ; Add a new field 
  [(δ rawset objref v_1 v_2 (osp_1 ...
                              (objref ((\{ field ... \}) any ...))
                              osp_2 ...))

   ((osp_1 ...
     (objref ((\{ (\[ v_1 \] = v_2) field ... \}) any ...))
     osp_2 ...) objref)

   (side-condition (not (equal? (term v_2)
                                (term nil))))]

  [(δ rawset objref v_1 nil (osp_1 ...
                              (objref ((\{ field ... \}) any ...))
                              osp_2 ...))

   ((osp_1 ... (objref ((\{ field ... \}) any ...)) osp_2 ...) objref)]

  ; Wrong arguments
  [(δ rawset v ... nil θ)
   (θ (δ error "rawset: missing arguments"))

   (side-condition (< (length (term (v ...)))
                      3))]
  
  ;                                                  
  ;                   ;;;                            
  ;                     ;                            
  ;                     ;                       ;    
  ;                     ;                       ;    
  ;    ;;;;    ;;;;     ;      ;;;;     ;;;   ;;;;;; 
  ;   ;    ;  ;;  ;;    ;     ;;  ;;   ;   ;    ;    
  ;   ;       ;    ;    ;     ;    ;  ;         ;    
  ;    ;;;;   ;;;;;;    ;     ;;;;;;  ;         ;    
  ;        ;  ;         ;     ;       ;         ;    
  ;   ;    ;  ;;   ;    ;     ;;   ;   ;   ;    ;    
  ;    ;;;;    ;;;;      ;;;   ;;;;     ;;;      ;;; 
  ;                                                  
  ;                                                  
  ;                                                  
  ; Index out of range
  [(δ select Number v ...)
   (δ error "bad argument #1 to 'select' (index out of range)")
   
   (where Number_2 ,(* -1 (length (term (v ...)))))
   
   (side-condition (or (< (term Number) (term Number_2))
                       (= (term Number) 0)))]
  
  ; Positive index, in the range [1;(length (sv ...))] 
  [(δ select Number v ...)
   ,(append (term (< ))
            (list-tail (term (v ...)) (term Number_2))
            (term ( >)))
   
   (side-condition (and (<= (term Number) (length (term (v ...))))
                        (<= 1 (term Number))))
   
   (where Number_2 ,(- (exact-floor (term Number)) 1))]
  
  ; Positive index > (length (sv ...))
  [(δ select Number v ...)
   (< >)
   
   (side-condition (> (term Number) (length (term (v ...)))))]
  
  ; Negative index
  [(δ select Number v ...)
   ,(append (term (< ))
            (list-tail (term (v ...)) (term Number_3))
            (term ( >)))
   
   (where Number_2 ,(* -1 (length (term (v ...)))))
   
   (side-condition (and (<= (term Number_2) (term Number))
                        (<= (term Number) -1)))
   
   (where Number_3 ,(+ (length (term (v ...)))
                       (exact-floor (term Number))))]
  
  ; Obtain the total number of actual arguments received.
  [(δ select "#" v ...)
   (< any >)
   
   (where any ,(length (term (v ...))))]
  
  [(δ select '#' v ...)
   (< any >)
   
   (where any ,(length (term (v ...))))]
  
  ; Default case
  [(δ select v_1 v ...)
   (δ error "bad argument #1 to 'select' (number expected)")]
  
  ;                                                                                                  
  ;                                                                           ;       ;;;            
  ;                                                                           ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;    ;;;;    ;;;;   ;;;;;;  ;;;;;;;  ;;;;   ;;;;;;    ;;;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
  ;   ;    ;  ;;  ;;    ;     ;  ;  ; ;;  ;;    ;      ;   ;    ;      ;   ;  ;;  ;;    ;     ;;  ;; 
  ;   ;       ;    ;    ;     ;  ;  ; ;    ;    ;          ;    ;          ;  ;    ;    ;     ;    ; 
  ;    ;;;;   ;;;;;;    ;     ;  ;  ; ;;;;;;    ;      ;;;;;    ;      ;;;;;  ;    ;    ;     ;;;;;; 
  ;        ;  ;         ;     ;  ;  ; ;         ;     ;    ;    ;     ;    ;  ;    ;    ;     ;      
  ;   ;    ;  ;;   ;    ;     ;  ;  ; ;;   ;    ;     ;   ;;    ;     ;   ;;  ;;  ;;    ;     ;;   ; 
  ;    ;;;;    ;;;;      ;;;  ;  ;  ;  ;;;;      ;;;   ;;; ;     ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
  ;                                                                                                  
  ;                                                                                                  
  ;
  
  ; Bad argument #1.
  [(δ setmetatable v_1 v_2 θ)
   (θ (δ error String_2))
   
   (where String_1 (δ type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 to 'setmetatable' (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {objref is a pointer to a table}
  ; Bad argument #2.
  [(δ setmetatable objref v θ)
   (θ (δ error "bad argument #2 to 'setmetatable' (nil or table expected)"))
   
   (where String_1 (δ type v))
   
   (side-condition (not (or (equal? (term String_1)
                                    "table")
                            
                            (equal? (term String_1)
                                    "nil"))))]
  
  ; {v points to a table or is nil}
  
  ; Protected meta-table
  [(δ setmetatable objref v θ)
   (θ (δ error "cannot change a protected metatable"))
   
   (side-condition (term (protectedMetaTable? objref θ)))]
  
  ; Non protected meta-table
  [(δ setmetatable tid v (osp_1 ...
                           (tid (evaluatedtable any pos_1))
                           osp_2 ...))
   ((osp_1 ...
     (tid (evaluatedtable v pos_2))
     osp_2 ...) tid)

   (where pos_2 (setFin tid v (osp_1 ...
                               (tid (evaluatedtable any pos_1))
                               osp_2 ...)))]
  
  
  ;                                                                  
  ;                                           ;                      
  ;                                           ;                      
  ;     ;                                     ;                      
  ;     ;                                     ;                      
  ;   ;;;;;;   ;;;;   ; ;;;   ;    ;  ;;;;;;; ;;;;;    ;;;;    ;;;;  
  ;     ;     ;;  ;;  ;;   ;  ;    ;  ;  ;  ; ;;  ;;  ;;  ;;   ;;  ; 
  ;     ;     ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;    ;   ;     
  ;     ;     ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;;;;;;   ;     
  ;     ;     ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;        ;     
  ;     ;     ;;  ;;  ;    ;  ;   ;;  ;  ;  ; ;;  ;;  ;;   ;   ;     
  ;      ;;;   ;;;;   ;    ;   ;;; ;  ;  ;  ; ;;;;;    ;;;;    ;     
  ;                                                                  
  ;                                                                  
  ;
  [(δ tonumber Number v)
   (δ tonumber String v)

   ; since Number could be in a non-decimal base v, it is easier to coerce it
   ; to string, and apply the same algorithm of conversion over it
   (where String (δ tostring Number ()))
   ]

  ; decimal number, no base specified
  [(δ tonumber String nil)
   Number
   
   ; though the manual does not specify this, in this case tonumber
   ; converts String following the rules of the lexer, as said by the semantics;
   ; however, lexer alone will not suffice: for example, in case of malformed
   ; strings beginning with a correct string representation of numbers.
   (where (any = Number) ,(with-handlers ([exn:fail?
                                      (λ (e) #f)])
                       ((λ ()
                          ; to use the parser, we need to feed it with an
                          ; statement...
                          ; NOTE: we append String directly. Then, the
                          ; conversion to a number is done by the lexer/parser.
                          (parse-this (string-append "_ENV = " (term String))
                                      #f
                                      (void))))))]

  ; {v ∉ String ∨ v cannot be coerced to a number}
  [(δ tonumber v nil)
   nil]

  ; base out of range
  [(δ tonumber v 1)
   (δ error String)

   (where String ,(string-append
                   "bad argument #2 to 'tonumber' (base out of range)"))]
  
  ; when called with a base (Number), then the first argument should be a string
  ; to be interpreted as an integer numeral in that base
  [(δ tonumber String Number)
   (convert_string String Number)]

;  ; it is also possible convert a number to another base
;  [(δ tonumber Number_1 Number_2)
;   (convert_number Number_1 Number_2)]

  ; {v_2 ≠ nil ∧ (v_1 ∉ String ∨ v_1 cannot be coerced to a number)}
  [(δ tonumber v_1 v_2)
   (δ error String)

   (where String ,(string-append
                   "bad argument #1 to 'tonumber' (string expected)"))]
  
  
  ;                                                                  
  ;                                             ;                    
  ;                                                                  
  ;     ;                       ;                                    
  ;     ;                       ;                                    
  ;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;   ;;;     ; ;;;    ;;;;; 
  ;     ;     ;;  ;;  ;    ;    ;      ;;  ;    ;     ;;   ;  ;;  ;; 
  ;     ;     ;    ;  ;         ;      ;        ;     ;    ;  ;    ; 
  ;     ;     ;    ;   ;;;;     ;      ;        ;     ;    ;  ;    ; 
  ;     ;     ;    ;       ;    ;      ;        ;     ;    ;  ;    ; 
  ;     ;     ;;  ;;  ;    ;    ;      ;        ;     ;    ;  ;;  ;; 
  ;      ;;;   ;;;;    ;;;;      ;;;   ;      ;;;;;   ;    ;   ;;; ; 
  ;                                                                ; 
  ;                                                            ;   ; 
  ;                                                             ;;;
  ; table objref has an associated metatable, with field ("__tostring" = any)
  [(δ tostring objref θ)
   (any (objref))

   ; index metatable of objref
   (where any (indexMetaTable objref "__tostring" θ))

   (side-condition (not (is_nil? (term any))))]
  
  ; v has an associated metatable, with field ("__tostring" = any)
  [(δ tostring v θ)
   (any (v))

   (side-condition (not (is_tid? (term v))))
   
   (where objref (getMetaTableRef v))
   
   (side-condition (term (refBelongsToTheta? objref θ)))

   ; directly index metatable objref
   (where any (δ rawget objref "__tostring" θ))

   (side-condition (not (is_nil? (term any))))]
  
  ; {meta-table of value's type is not set ∨ meta-table does not have
  ; __tostring field ∨ v ∈ objref}
  
  ; implement custom conversions to string
  ; simple way of avoiding exception check, when using inexact->exact, below
  [(δ tostring +nan.0 θ)
   "nan"]

  [(δ tostring +inf.0 θ)
   "inf"]

  [(δ tostring -inf.0 θ)
   "-inf"]
  
  ; to implement the behaviour of Lua's coercion: 1.0 .. 1.0 = "11"
  [(δ tostring Number θ)
   ; {Number ∉ {nan, inf}}
   ,(~a (inexact->exact (term Number)))

   (side-condition (= (floor (term Number))
                      (term Number)))]
  
  [(δ tostring Number θ)
   ,(~a (term Number))

   (side-condition (not (= (floor (term Number))
                           (term Number))))]
  
  [(δ tostring v θ)
   String_2

   ; v is a table or closure id
   (side-condition (or (is_tid? (term v))
                       (is_cid? (term v))))
   
   (where String_1 (δ type v))
   
   (where String_2 ,(string-append (term String_1) ": "
                                   (~a (term v))))]

  ; {v ∉ tid ∪ cid}
  ; default case: racket/format conversion
  [(δ tostring v θ)
   ; From racket/format
   ,(~a (term v))]
  
  ;                                  
  ;                                  
  ;                                  
  ;     ;                            
  ;     ;                            
  ;   ;;;;;;  ;    ;  ;;;;;    ;;;;  
  ;     ;      ;   ;  ;;  ;;  ;;  ;; 
  ;     ;      ;  ;   ;    ;  ;    ; 
  ;     ;      ;  ;   ;    ;  ;;;;;; 
  ;     ;       ; ;   ;    ;  ;      
  ;     ;       ;;    ;;  ;;  ;;   ; 
  ;      ;;;     ;    ;;;;;    ;;;;  
  ;              ;    ;              
  ;             ;     ;              
  ;            ;;     ;              

  [(δ type Number )
   ,(term "number")]
  
  [(δ type nil)
   ,(term "nil")]
  
  [(δ type Boolean)
   ,(term "boolean")]
  
  [(δ type String)
   ,(term "string")]
  
  [(δ type cid)
   ,(term "function")]
  
  ; Default case
  [(δ type objref)
   ,(term "table")]
  
  ;                                                  
  ;                                   ;;;     ;;;    
  ;                                     ;       ;    
  ;                                     ;       ;    
  ;                                     ;       ;    
  ;   ;;  ;;  ;;;;;     ;;;     ;;;     ;       ;    
  ;    ;  ;   ;;  ;;   ;   ;   ;   ;    ;       ;    
  ;     ;;    ;    ;  ;            ;    ;       ;    
  ;     ;;    ;    ;  ;        ;;;;;    ;       ;    
  ;     ;;    ;    ;  ;       ;    ;    ;       ;    
  ;    ;  ;   ;;  ;;   ;   ;  ;   ;;    ;       ;    
  ;   ;;  ;;  ;;;;;     ;;;    ;;; ;     ;;;     ;;; 
  ;           ;                                      
  ;           ;                                      
  ;           ;                                      
  [(δ xpcall v_1 v_2 v_3 ...)
   ((v_1 (v_3 ...)) ProtectedMode v_2)]

  ;                                  
  ;                           ;      
  ;                           ;      
  ;                     ;     ;      
  ;                     ;     ;      
  ;   ;;;;;;;   ;;;   ;;;;;;  ; ;;;  
  ;   ;  ;  ;  ;   ;    ;     ;;   ; 
  ;   ;  ;  ;      ;    ;     ;    ; 
  ;   ;  ;  ;  ;;;;;    ;     ;    ; 
  ;   ;  ;  ; ;    ;    ;     ;    ; 
  ;   ;  ;  ; ;   ;;    ;     ;    ; 
  ;   ;  ;  ;  ;;; ;     ;;;  ;    ; 
  ;                                  
  ;                                  
  ;

  
  ;                          
  ;           ;              
  ;           ;              
  ;           ;              
  ;           ;              
  ;     ;;;   ;;;;;    ;;;;  
  ;    ;   ;  ;;  ;;  ;    ; 
  ;        ;  ;    ;  ;      
  ;    ;;;;;  ;    ;   ;;;;  
  ;   ;    ;  ;    ;       ; 
  ;   ;   ;;  ;;  ;;  ;    ; 
  ;    ;;; ;  ;;;;;    ;;;;  
  ;                          
  ;                          
  ;
  [(δ math.abs String)
   (δ math.abs Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.abs Number )
   ,(abs (term Number))]
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;     ;;;     ;;;    ;;;;    ;;;;  
  ;    ;   ;   ;   ;  ;;  ;;  ;    ; 
  ;        ;  ;       ;    ;  ;      
  ;    ;;;;;  ;       ;    ;   ;;;;  
  ;   ;    ;  ;       ;    ;       ; 
  ;   ;   ;;   ;   ;  ;;  ;;  ;    ; 
  ;    ;;; ;    ;;;    ;;;;    ;;;;  
  ;                                  
  ;                                  
  ;
  [(δ math.acos String)
   (δ math.acos Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.acos Number_1)
   Number_2

   ; check that the result is in the set of real numbers
   (where Number_2 ,(acos (term Number_1)))]

  ; parameter outside of [-1;1]
  [(δ math.acos Number)
   +nan.0]
  
  ;                                  
  ;                     ;            
  ;                                  
  ;                                  
  ;                                  
  ;     ;;;    ;;;;   ;;;     ; ;;;  
  ;    ;   ;  ;    ;    ;     ;;   ; 
  ;        ;  ;         ;     ;    ; 
  ;    ;;;;;   ;;;;     ;     ;    ; 
  ;   ;    ;       ;    ;     ;    ; 
  ;   ;   ;;  ;    ;    ;     ;    ; 
  ;    ;;; ;   ;;;;   ;;;;;   ;    ; 
  ;                                  
  ;                                  
  ;
  [(δ math.asin String)
   (δ math.asin Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.asin Number_1)
   Number_2

   ; check that the result is in the set of real numbers (for parameter outside
   ; of [-1;1], the result will be a number with a non-zero complex part
   (where Number_2 ,(asin (term Number_1)))]

  ; parameter outside of [-1;1]
  [(δ math.asin Number)
   +nan.0]
  
  ;                                  
  ;                                  
  ;                                  
  ;             ;                    
  ;             ;                    
  ;     ;;;   ;;;;;;    ;;;   ; ;;;  
  ;    ;   ;    ;      ;   ;  ;;   ; 
  ;        ;    ;          ;  ;    ; 
  ;    ;;;;;    ;      ;;;;;  ;    ; 
  ;   ;    ;    ;     ;    ;  ;    ; 
  ;   ;   ;;    ;     ;   ;;  ;    ; 
  ;    ;;; ;     ;;;   ;;; ;  ;    ; 
  ;                                  
  ;                                  
  ;
  [(δ math.atan String)
   (δ math.atan Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.atan Number)
   ,(atan (term Number))]

  
  ;                                  
  ;                     ;     ;;;    
  ;                             ;    
  ;                             ;    
  ;                             ;    
  ;     ;;;    ;;;;   ;;;       ;    
  ;    ;   ;  ;;  ;;    ;       ;    
  ;   ;       ;    ;    ;       ;    
  ;   ;       ;;;;;;    ;       ;    
  ;   ;       ;         ;       ;    
  ;    ;   ;  ;;   ;    ;       ;    
  ;     ;;;    ;;;;   ;;;;;      ;;; 
  ;                                  
  ;                                  
  ;
  [(δ math.ceil String)
   (δ math.ceil Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.ceil Number)
   ,(ceiling (term Number))]
  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;     ;;;    ;;;;    ;;;;  
  ;    ;   ;  ;;  ;;  ;    ; 
  ;   ;       ;    ;  ;      
  ;   ;       ;    ;   ;;;;  
  ;   ;       ;    ;       ; 
  ;    ;   ;  ;;  ;;  ;    ; 
  ;     ;;;    ;;;;    ;;;;  
  ;                          
  ;                          
  ;
  [(δ math.cos String)
   (δ math.cos Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.cos Number )
   ,(cos (term Number))]

  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;     ;;;    ;;;;    ;;;;   ; ;;;  
  ;    ;   ;  ;;  ;;  ;    ;  ;;   ; 
  ;   ;       ;    ;  ;       ;    ; 
  ;   ;       ;    ;   ;;;;   ;    ; 
  ;   ;       ;    ;       ;  ;    ; 
  ;    ;   ;  ;;  ;;  ;    ;  ;    ; 
  ;     ;;;    ;;;;    ;;;;   ;    ; 
  ;                                  
  ;                                  
  ;
  [(δ math.cosh String)
   (δ math.cosh Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.cosh Number )
   ,(cosh (term Number))]
  
  ;                          
  ;        ;                 
  ;        ;                 
  ;        ;                 
  ;        ;                 
  ;    ;;;;;   ;;;;    ;;;;; 
  ;   ;;  ;;  ;;  ;;  ;;  ;; 
  ;   ;    ;  ;    ;  ;    ; 
  ;   ;    ;  ;;;;;;  ;    ; 
  ;   ;    ;  ;       ;    ; 
  ;   ;;  ;;  ;;   ;  ;;  ;; 
  ;    ;;;;;   ;;;;    ;;; ; 
  ;                        ; 
  ;                    ;   ; 
  ;                     ;;;
  [(δ math.deg String)
   (δ math.deg Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.deg Number)
   ,(radians->degrees (term Number))]

  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;; 
  ;   ;    ;    ;;    ;    ; 
  ;   ;;;;;;    ;;    ;    ; 
  ;   ;         ;;    ;    ; 
  ;   ;;   ;   ;  ;   ;;  ;; 
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;                   ;      
  ;                   ;      
  ;                   ;
  [(δ math.exp String)
   (δ math.exp Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.exp Number)
   ,(exp (term Number))]

  ;                                          
  ;      ;;   ;;;                            
  ;     ;       ;                            
  ;     ;       ;                            
  ;     ;       ;                            
  ;   ;;;;;     ;      ;;;;    ;;;;    ;;;;  
  ;     ;       ;     ;;  ;;  ;;  ;;   ;;  ; 
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;;  ;;  ;;  ;;   ;     
  ;     ;        ;;;   ;;;;    ;;;;    ;     
  ;                                          
  ;                                          
  ;                                          
  [(δ math.floor String)
   (δ math.floor Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.floor Number )
   ,(floor (term Number))]
  
  ;                                  
  ;      ;;                        ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;   ;;;;;   ;;;;;;;  ;;;;    ;;;;; 
  ;     ;     ;  ;  ; ;;  ;;  ;;  ;; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;;  ;;  ;;  ;; 
  ;     ;     ;  ;  ;  ;;;;    ;;;;; 
  ;                                  
  ;                                  
  ;                                  
  [(δ math.fmod Number_1 ... String v ...)
   (δ math.fmod Number_1 ... Number_2 v ...)
   
   (where Number_2 (δ tonumber String nil))]
  
  [(δ math.fmod Number_1 0)
   -nan.0]

  ; {Number_2 ≠ 0}
  [(δ math.fmod Number_1 Number_2)
   ,(exact-floor (remainder (term Number_1) (term Number_2)))]
  
  ;                          
  ;   ;;;                    
  ;     ;                    
  ;     ;                    
  ;     ;                    
  ;     ;      ;;;;    ;;;;; 
  ;     ;     ;;  ;;  ;;  ;; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;;  ;;  ;;  ;; 
  ;      ;;;   ;;;;    ;;; ; 
  ;                        ; 
  ;                    ;   ; 
  ;                     ;;;
  [(δ math.log Number_1 ... String v ...)
   (δ math.log Number_1 ... Number_2 v ...)
   
   (where Number_2 (δ tonumber String nil))]

  [(δ math.log Number_1 nil)
   ,(log (term Number_2))

   (where Number_2 ,(exact->inexact (term Number_1)))]

  [(δ math.log Number_1 Number_2)
   (δ /
      (δ math.log Number_3 nil)
      (δ math.log Number_4 nil))

   (where Number_3 ,(exact->inexact (term Number_1)))
   (where Number_4 ,(exact->inexact (term Number_2)))]

  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;   ;;;;;;;   ;;;   ;;  ;; 
  ;   ;  ;  ;  ;   ;   ;  ;  
  ;   ;  ;  ;      ;    ;;   
  ;   ;  ;  ;  ;;;;;    ;;   
  ;   ;  ;  ; ;    ;    ;;   
  ;   ;  ;  ; ;   ;;   ;  ;  
  ;   ;  ;  ;  ;;; ;  ;;  ;; 
  ;                          
  ;                          
  ;
  [(δ math.max Number_1 ... String v ...)
   (δ math.max Number_1 ... Number_2 v ...)
   
   (where Number_2 (δ tonumber String nil))]
  
  [(δ math.max Number ...)
   ,(foldr (λ (nmbr accum) (max nmbr accum))
           -inf.0
           (term (Number ...)))]
  ;                                  
  ;                        ;     ;;  
  ;                        ;    ;    
  ;                        ;    ;    
  ;                        ;    ;    
  ;   ;;;;;;;  ;;;;    ;;;;;  ;;;;;  
  ;   ;  ;  ; ;;  ;;  ;;  ;;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;;  ;;  ;;  ;;    ;    
  ;   ;  ;  ;  ;;;;    ;;;;;    ;    
  ;                                  
  ;                                  
  ;
  [(δ math.modf String)
   (δ math.modf Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.modf Number)
   ,(- (term Number) (exact-truncate (term Number)))]
  ;                          
  ;                        ; 
  ;                        ; 
  ;                        ; 
  ;                        ; 
  ;    ;;;;     ;;;    ;;;;; 
  ;    ;;  ;   ;   ;  ;;  ;; 
  ;    ;           ;  ;    ; 
  ;    ;       ;;;;;  ;    ; 
  ;    ;      ;    ;  ;    ; 
  ;    ;      ;   ;;  ;;  ;; 
  ;    ;       ;;; ;   ;;;;; 
  ;                          
  ;                          
  ;                          
  [(δ math.rad String)
   (δ math.rad Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.rad Number)
   ,(degrees->radians (term Number))]
  
  ;                          
  ;             ;            
  ;                          
  ;                          
  ;                          
  ;    ;;;;   ;;;     ; ;;;  
  ;   ;    ;    ;     ;;   ; 
  ;   ;         ;     ;    ; 
  ;    ;;;;     ;     ;    ; 
  ;        ;    ;     ;    ; 
  ;   ;    ;    ;     ;    ; 
  ;    ;;;;   ;;;;;   ;    ; 
  ;                          
  ;                          
  ;                          
  [(δ math.sin String)
   (δ math.sin Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.sin Number)
   ,(sin (term Number))]
  
  ;                                  
  ;             ;             ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;    ;;;;   ;;;     ; ;;;   ; ;;;  
  ;   ;    ;    ;     ;;   ;  ;;   ; 
  ;   ;         ;     ;    ;  ;    ; 
  ;    ;;;;     ;     ;    ;  ;    ; 
  ;        ;    ;     ;    ;  ;    ; 
  ;   ;    ;    ;     ;    ;  ;    ; 
  ;    ;;;;   ;;;;;   ;    ;  ;    ; 
  ;                                  
  ;                                  
  ;
  [(δ math.sinh String)
   (δ math.sinh Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.sinh Number)
   ,(sinh (term Number))]


  
  ;                                  
  ;                                  
  ;                                  
  ;                             ;    
  ;                             ;    
  ;    ;;;;    ;;;;;   ;;;;   ;;;;;; 
  ;   ;    ;  ;;  ;;   ;;  ;    ;    
  ;   ;       ;    ;   ;        ;    
  ;    ;;;;   ;    ;   ;        ;    
  ;        ;  ;    ;   ;        ;    
  ;   ;    ;  ;;  ;;   ;        ;    
  ;    ;;;;    ;;; ;   ;         ;;; 
  ;                ;                 
  ;                ;                 
  ;                ;
  [(δ math.sqrt String)
   (δ math.sqrt Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.sqrt Number)
   ,(sqrt (term Number))

   (side-condition (> (term Number) 0))]

  ; {Number >= 0}
  [(δ math.sqrt Number)
   -nan.0]
  
  ;                          
  ;                          
  ;                          
  ;     ;                    
  ;     ;                    
  ;   ;;;;;;    ;;;   ; ;;;  
  ;     ;      ;   ;  ;;   ; 
  ;     ;          ;  ;    ; 
  ;     ;      ;;;;;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;   ;;  ;    ; 
  ;      ;;;   ;;; ;  ;    ; 
  ;                          
  ;                          
  ;
  [(δ math.tan String)
   (δ math.tan Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.tan Number)
   ,(tan (term Number))]
  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;     ;                     ;      
  ;     ;                     ;      
  ;   ;;;;;;    ;;;   ; ;;;   ; ;;;  
  ;     ;      ;   ;  ;;   ;  ;;   ; 
  ;     ;          ;  ;    ;  ;    ; 
  ;     ;      ;;;;;  ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ;  ;    ; 
  ;     ;     ;   ;;  ;    ;  ;    ; 
  ;      ;;;   ;;; ;  ;    ;  ;    ; 
  ;                                  
  ;                                  
  ;
  [(δ math.tanh String)
   (δ math.tanh Number)
   
   (where Number (δ tonumber String nil))]
  
  [(δ math.tanh Number)
   ,(tanh (term Number))]

  ; Default case of math functions
  [(δ builtinserv v ...)
   (δ error any)

   (side-condition (string-prefix? (symbol->string (term builtinserv))
                                   "math."))

   (where any ,(string-append (symbol->string (term builtinserv))
                              ": bad argument #1 (number expected)"))
   ]
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
  [(δ string.dump cid (osp_1 ... (cid functiondef) osp_2 ...))
   String
   
   (where String ,(str-flatten (term functiondef)))]

  [(δ string.dump v θ)
   any
   
   (where any (δ error ,(string-append
                         "bad argument #1 (function expected, got "
                         (term (δ type v))
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
  [(δ string.len Number)
   (δ string.len String)

   (where String (δ tostring Number ()))]
  
  [(δ string.len String)
   (δ \# String)]
  
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
  [(δ string.rep Number_1 Number_2 v)
   (δ string.rep String Number_2 v)

   (where String (δ tostring Number_1 ()))]

  [(δ string.rep String_1 Number_1 Number_2)
   (δ string.rep String_1 Number_1 String_2)

   (where String_2 (δ tostring Number_2 ()))]
  
  [(δ string.rep String 1 any)
   String]
  
  [(δ string.rep String Number nil)
   any
   
   (where any ,(foldr (λ (str accum) (term (δ .. ,str ,accum)))
                      (term String)
                      (build-list
                       (- ; build-list contract: exact-nonnegative-integer?
                        (inexact->exact (term Number)) 1)
                       (λ (nmbr) (term String)))))
   ]

  ; {v_2 != nil}
  [(δ string.rep String_1 Number String_2)
   (δ .. any String_1)
   
   (where any ,(foldr (λ (str accum) (term (δ .. (δ .. String_1 String_2)
                                                  ,accum)))
                      (term (δ .. String_1 String_2))
                      (build-list (- (term Number) 2)
                                  (λ (nmbr) (term String_1)))))
   ]

  [(δ string.rep v_1 v_2 v_3)
   (δ error "string.rep: arguments of the wrong type")]
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
  [(δ string.reverse Number)
   (δ string.reverse String)

   (where String (δ tostring Number ()))]

  [(δ string.reverse String)
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
  [(δ string.sub Number_1 Number_2 Number_3)
   (δ string.sub String Number_2 Number_3)

   (where String (δ tostring Number_1 ()))]
  
  ; correction of indices
  ; Number_1 < 0
  [(δ string.sub String Number_1 Number_2)
   (δ string.sub String 1 Number_2)

   (side-condition (< (term Number_1)
                      0))]
  
  ; If Number_2 is greater than the string length, it is corrected to that length
  [(δ string.sub String Number_1 Number_2)
   (δ string.sub String Number_1 (δ \# String))

   (side-condition (< (term (δ \# String))
                      (exact-floor (term Number_2))))]

  ; If, after these corrections, Number_1 is greater than Number_2, the function
  ; returns the empty string. 
  [(δ string.sub String Number_1 Number_2)
   ""

   (side-condition (< (term Number_2)
                      (term Number_1)))]

  ; Normal case
  [(δ string.sub String Number_1 Number_2)
   ,(substring (term String)
               (- (exact-floor (term Number_1)) 1)
               (exact-floor (term Number_2)))]
  
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
  [(δ table.concat objref nil v_1 v_2 θ)
   (δ table.concat objref "" v_1 v_2 θ)]

  [(δ table.concat objref String nil v θ)
   (δ table.concat objref String 1 v θ)]

  [(δ table.concat objref_1 String v nil θ)
   (δ table.concat objref_1
                    String
                    v
                    (δ \# evaluatedtable)
                    θ)

   (where ((objref_2 object_2) ...
           (objref_1 (evaluatedtable any_1 any_2))
           (objref_3 object_3) ...) θ)]
  
  ; simpler case
  [(δ table.concat objref String Number_1 Number_2 θ)
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
  [(δ table.concat objref String Number_1 Number_2 θ)
   ""]

  ; wrong parameters
  [(δ table.concat v_1 v_2 ... θ)
   (δ error "table.concat: table expected")

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
  [(δ table.insert objref_1 nil v
                    ((objref_2 object_2) ...
                     (objref_1 ((\{ field ... \}) any ...))
                     (objref_3 object_3) ...))

   (((objref_2 object_2) ...
     (objref_1 ((\{ field ... (\[  Number \] = v) \}) any ...))
     (objref_3 object_3) ...) (< >))
   
   (where Number (δ \# (\{ field ... \})))]
  
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
  
  
  [(δ table.pack v ...)
   any_3
   
   (where Number ,(length (term (v ...))))
   
   ; Take the list of keys (naturals starting from 1),
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
  [(δ table.unpack objref_1 v_1 v_2 v_3 ... θ)
   any_2
   
   ;   (where ((objref_2 object_2) ...
   ;           (objref_1 (evaluatedtable any_1 Number))
   ;           (objref_3 object_3) ...)
   ;          θ)
   (where evaluatedtable (getTable objref_1 θ))

   ; Set range of indexes. v_1 and v_2 should be nil or a number 
   (where Number_1 ,(if (not (equal? (term v_1)
                                     (term nil)))
                        (term v_1)
                        1) ; Default first index
          )
   
   (where Number_2 ,(if (not (equal? (term v_2)
                                     (term nil)))
                        (term v_2)
                        (term (δ \# evaluatedtable)) ; Default last index
                        ))

   ; Construct a tuple of table indexing expressions
   (where any_2 ,(append (term (< ))

                         (map (λ (index)
                                (append (term (objref_1 \[ ))
                                        (term (,index))
                                        (term (\]))))
                              
                              (range (exact-floor (term Number_1))
                                     (+ (exact-floor (term Number_2)) 1)))

                         (term ( >))))]

  ; erroneous cases
  [(δ table.unpack v_1 v_2 ... θ)
   (δ error String_2)
   
   (where String_1 (δ type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append "bad argument #1 (table expected, got "
                                   (term String_1)
                                   ")"))]
  
  [(δ table.unpack v_1 v_2 v_3 ... θ)
   (δ error String_2)
   
   (where String_1 (δ type v_2))
   
   (side-condition (not (equal? (term String_1)
                                "number")))
   
   (where String_2 ,(string-append "bad argument #2 (number expected, got "
                                   (term String_1)
                                   ")"))]
  
  ; Default case
  [(δ table.unpack v_1 v_2 v_3 v_4 ... θ)
   (δ error String_2)
   
   (where String_1 (δ type v_3))
   
   (where String_2 ,(string-append "bad argument #3 (number expected, got "
                                   (term String_1)
                                   ")"))]
  
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
  ; Basic implementation of the require service
  [(δ package.require String_1 θ)
   (δ load String_2 nil nil nil θ)

   (where String_2 ,(file->string (term String_1)))]

  
  ;                                                                                                  
  ;        ;             ;;                   ;;;                                                    
  ;        ;            ;                       ;                                                    
  ;        ;            ;                       ;       ;                                            
  ;        ;            ;                       ;       ;                                            
  ;    ;;;;;   ;;;;   ;;;;;     ;;;   ;    ;    ;     ;;;;;;            ;;;     ;;;    ;;;;    ;;;;  
  ;   ;;  ;;  ;;  ;;    ;      ;   ;  ;    ;    ;       ;              ;   ;   ;   ;  ;    ;  ;;  ;; 
  ;   ;    ;  ;    ;    ;          ;  ;    ;    ;       ;             ;            ;  ;       ;    ; 
  ;   ;    ;  ;;;;;;    ;      ;;;;;  ;    ;    ;       ;             ;        ;;;;;   ;;;;   ;;;;;; 
  ;   ;    ;  ;         ;     ;    ;  ;    ;    ;       ;             ;       ;    ;       ;  ;      
  ;   ;;  ;;  ;;   ;    ;     ;   ;;  ;   ;;    ;       ;              ;   ;  ;   ;;  ;    ;  ;;   ; 
  ;    ;;;;;   ;;;;     ;      ;;; ;   ;;; ;     ;;;     ;;;            ;;;    ;;; ;   ;;;;    ;;;;  
  ;                                                                                                  
  ;                                                                                                  
  ;                                                                                                  

  ; To capture the "no value" error for every builtinserv 
  [(δ builtinserv v ...)
   (δ error any)

   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]

  ; Services that don't modify theta
  [(δ builtinserv v ... θ)
   (δ error any)

   (side-condition (member (term builtinserv)
                           (term (; basic functions
                                  ipairs
                                  next
                                  pairs
                                  load
                                  loadfile
                                  getmetatable
                                  tostring
                                  rawget
                                  rawlen
                                  ; package
                                  require
                                  ; string
                                  string.dump
                                  ; table
                                  table.concat
                                  table.unpack))))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]

  ; Services that modify theta
  [(δ builtinserv v ... θ)
   (θ (δ error any))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]
  )


; To convert booleans values in racket to boolean values in our language 
(define-metafunction ext-lang
  
  [(toBool #t)
   true]
  
  [(toBool any)
   false])

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

; Returns the predefined location where a meta-table for an indicated type,
; different from type "table", must be stored
(define-metafunction ext-lang
  [(getMetaTableRef Number)
   (objr 1)]
  
  [(getMetaTableRef nil)
   (objr 2)]
  
  [(getMetaTableRef Boolean)
   (objr 3)]
  
  [(getMetaTableRef String)
   (objr 4)]
  
  [(getMetaTableRef cid)
   (objr 5)])

(provide getMetaTableRef)


; Meta-function that tries to get the meta-table of a given value and index it
; with a given key. If it doesn't succeed, it returns nil.
; PRE : {sv_1 is the value whose meta-table we want to index and sv_2 is the
;        key}
; ret = (indexMetaTable sv_1 sv_2 θ)
(define-metafunction ext-lang
  ; objref_1 points to a table, which has a metatable with key v_1
  [(indexMetaTable objref_1 v_1 (osp_1 ...
                                 (objref_1 (evaluatedtable objref_2 any))
                                 osp_2 ...))
   v_2
   
   (where v_2 (δ rawget objref_2 v_1 (osp_1 ...
                                       (objref_1 (evaluatedtable objref_2 any))
                                       osp_2 ...)))]

  ; objref_1 points to a table, which hasn't a metatable
  [(indexMetaTable objref v θ)
   nil]
  
  ; v_1 isn't a table and doesn't have a metatable.
  [(indexMetaTable v_1 v_2 θ)
   nil
   
   (where objref (getMetaTableRef v_1))
   
   (side-condition (not (term (refBelongsToTheta? objref θ))))]
  
  ; v_1 isn't a table and has a metatable.
  [(indexMetaTable v_1 v_2 θ)
   v_3
   
   (where objref (getMetaTableRef v_1))
   
   (where v_3 (δ rawget objref v_2 θ))])

(provide indexMetaTable)


(define-metafunction ext-lang
  ; Protected meta-table
  [(protectedMetaTable? objref θ)
   #t
   
   (side-condition (not (equal? (term (indexMetaTable objref "__metatable" θ))
                                (term nil))))]
  
  ; Default case
  [(protectedMetaTable? objref θ)
   #f])

(provide protectedMetaTable?)


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
  
  [(errmessage WrongFunCall String)
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
  keyBelongsTo? : tableconstructor v -> any
  
  [(keyBelongsTo? (\{ field ... (\[ v_1 \] = v_2) field ... \}) v_3)
   #t

   (side-condition (equal? (term (δ == v_1 v_3))
                           'true))]
  
  ; Default case
  [(keyBelongsTo? tableconstructor v)
   #f])

; To ease the comparation of the keys of a table, reusing (δ == ...)  
(define-metafunction ext-lang
  extractField : (field ...) v -> any
  
  [(extractField (field ...) v)
   ,(filter (λ (field)
              ; Take each field that equals to
              ; (\[ v \] = any), according to delta
              ((λ (match)
                 (if match
                     
                     ; There must be just one match structure
                     ((λ (bindings)
                        ; Compare the keys
                        (if (equal? (term (δ == ,(bind-exp (list-ref bindings 0)) v))
                                    (term true))
                            #t
                            #f))
                      ; Pass the only match structure obtained
                      (match-bindings (list-ref match 0)))
                     
                     #f))
               ; Extract each component of the field
               (redex-match ext-lang
                            (\[ any_1 \] = any_2)
                            field)))
            (term (field ...)))])