#lang racket
(require redex
         "../grammar.rkt"
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
  [(δbasic binop v_1 v_2)
   (δbasic binop Number_1 Number_2)
   
   (side-condition (and
                    ; the following condition triggers coercion
                    (or (is_string? (term v_1))
                        (is_string? (term v_2)))
                    
                    (term (isArithBinOp binop))))
   
   (where Number_1 (δbasic tonumber v_1 nil))
   (where Number_2 (δbasic tonumber v_2 nil))]
  
  ; from https://www.lua.org/manual/5.2/manual.html#2.1:
  ; "Number represents real (double-precision floating-point) numbers";
  ; we reuse racket's implementation of double-precision IEEE floating-point
  ; numbers, flonums 
  [(δbasic + Number_1 Number_2)
   ,(+ (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic - Number_1 Number_2)
   ,(- (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic * Number_1 Number_2)
   ,(* (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
 
  [(δbasic / Number_1 Number_2)
   ,(/ (term Number_3) (term Number_4))

   ; to guarantee ieee 754 behavior, we apply a step of conversion to flonums;
   ; hence, for example, a division (/ 1 0) results in +inf.0, as in Lua,
   ; instead of raising an exception
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic ^ Number_1 Number_2)
   ,(expt (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic % Number_1 Number_2)
   ;a - math.floor(a/b)*b
   (δbasic - Number_3
           (δbasic *
                   ; NOTE: this should be δmath math.floor, but we cannot include
                   ; the module deltaMath, or else it would introduce a loop in
                   ; modules' dependencies
                   ,(floor (term (δbasic / Number_3 Number_4)))
                   Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]

  [(δbasic - String)
   (δbasic - Number)
   
   (where Number (δbasic tonumber String nil))]

  [(δbasic - Number)
   ,(- (term Number))
   
   (where Number_2 ,(real->double-flonum (term Number)))]
  
  ; Number comparison
  [(δbasic < Number_1 Number_2)
   (toBool ,(< (term Number_1) (term Number_2)))]
  
  [(δbasic <= Number_1 Number_2)
   (toBool ,(<= (term Number_1) (term Number_2)))]
  
  ; string comparison
  [(δbasic < String_1 String_2)
   (toBool ,(string<? (term String_1) (term String_2)))]
  
  [(δbasic <= String_1 String_2)
   (toBool ,(string<=? (term String_1) (term String_2)))]
  
  ; string concatenation
  ; coercion
  [(δbasic .. Number v)
   (δbasic .. String v)

   ; perform coercion only if the second parameter is also a string or a number
   (side-condition (or (is_string? (term v))
                       (is_number? (term v))))
   
   (where String (δbasic tostring Number ()))]

  [(δbasic .. String_1 Number)
   (δbasic .. String_1 String_2)
   
   (where String_2 (δbasic tostring Number ()))]
  
  [(δbasic .. String_1 String_2)
   ,(string-append (term String_1) (term String_2))]
  
  ; string length
  ; Racket's bytes-string, to simulate what Lua's # operator does.
  [(δbasic \# String)
   ,(bytes-length (string->bytes/utf-8 (term String)))]
  
  ; Table length: just the max. numeric key
  [(δbasic \# (\{ field ... \}))
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
  [(δbasic \# evaluatedtable)
   0]
  
  ; equality comparison
  ; numbers needs special treatment
  [(δbasic == Number_1 Number_2)
   (toBool ,(= (term Number_1) (term Number_2)))]
  
  [(δbasic == v_1 v_2)
   (toBool ,(equal? (term v_1) (term v_2)))]
  
  ; logical connectives
  [(δbasic and v e)
   v
   (side-condition (is_false_cond? (term v)))]
  
  ; try: a,b = true and g(), with g being a function that returns 2 or more 
  ; values
  [(δbasic and v e) 
   (\( e \))
   (side-condition (not (is_false_cond? (term v))))]
  
  [(δbasic or v e)
   v
   (side-condition (not (is_false_cond? (term v))))]
  
  ; Try: a,b = false or g(), with g being a function that returns 2 or more 
  ; values
  [(δbasic or v e)
   (\( e \))
   (side-condition (is_false_cond? (term v)))]
  
  [(δbasic not v)
   true

   ; v = nil, false?
   (side-condition (is_false_cond? (term v)))]
  
  [(δbasic not v) 
   false]

   ; default case of binop and unop
  [(δbasic any v ...)
   (δbasic error String)

   (side-condition (or (redex-match ext-lang unop (term any))
                       (redex-match ext-lang binop (term any))))
                               
   
   (where String ,(string-append (symbol->string (term any))
                              ": erroneous parameters"))]

    
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
  [(δbasic assert v_1 v_2 v_3 ...)
   (δbasic error any)
   
   (side-condition (is_false_cond? (term v_1)))
   
   (where any ,(if (equal? (term v_2) (term nil))
                   ; v_2 is nil. Return default error message.
                   (term "assertion failed!")
                   (term v_2)))]
  
  ; Assert: condition evaluates to true
  [(δbasic assert v ...)
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
  [(δbasic collectgarbage σ_1 θ_1 s)
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
  
  [(δbasic error v)
   ($err v)]
  
  ; TODO: model level
  [(δbasic error v_1 v_2)
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
  ; table value has a meta-table, which has a "__metatable" key.
  [(δbasic getmetatable objref_1 (osp_1 ...
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

   (side-condition (not (is_nil? (term v))))
   ]

  ; table value doesn't have a protected meta-table
  [(δbasic getmetatable objref (osp_1 ...
                            (objref (tableconstructor v any))
                            osp_2 ...))
   v]

  
  ; the value isn't a table. It has a meta-table, which has
  ; a "__metatable" key,
  [(δbasic getmetatable any_1 (osp_1 ...
                           (objref_2 ((\{ field ...
                                          (\[ "__metatable" \] = v)
                                          field ... \})
                                      any_2 ...))
                           osp_2 ...))
   v
   
   (where objref_2 (getMetaTableRef any_1))
   ]
  
  ; the value isn't a table. It has a meta-table, which has not a "__metatable"
  ; key
  [(δbasic getmetatable any_1 (osp_1 ...
                           (objref_2 any_2)
                           osp_2 ...))
   objref_2
   
   (where objref_2 (getMetaTableRef any_1))
   ]
  
  ; the value isn't a table. Its type has not a meta-table set.
  [(δbasic getmetatable any θ)
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
  
  ; custom iterator, provided by the metatable
  [(δbasic ipairs objref θ)
   ((function $IpairsCustomIter ()
              (local v1 v2 v3 = (any (objref))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (where any (indexMetaTable objref "__ipairs" θ))
   
   (side-condition (not (equal? (term any)
                                (term nil))))]

  ; default iterator
  [(δbasic ipairs objref θ)
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
  
  [(δbasic ipairs v ... θ)
   (δbasic error "bad argument #1 (table expected)")]
  
  
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
  [(δbasic load String v_1 "b" v_3)
   any

   ; String should be a string (flattened) representation of a function
   ; (obtained through string.dump). We reuse Racket's reader to
   ; try to parse it and obtain the corresponding redex term.
   (where any ,(with-handlers
                     ([exn:fail?
                       ; the string cannot be parsed
                       (λ (e) (term (< nil
                                       "attempt to load a text chunk (mode is 'b')"
                                       >)))])
                   
                   (read (open-input-string (term String)))
                   ))
   ]
 
  ; Lua program expressed into a string, either syntactically correct or not 
  [(δbasic load String v_1 "t" v_3)
   any_2
   
   (where any_1 ,(with-handlers
                     ([exn:fail?
                       ; the string cannot be parsed
                       (λ (e) (append (term (< nil ))
                                      (list (if (is_nil? (term v_1))
                                                
                                                (string-append "[string "
                                                               (term String)
                                                               "]")
                                                           
                                                (term v_1)))
                                      (term ( >))))])
                   
                   (parse-this (term String)
                               #t
                               ; TODO: the reference to _G is hardcoded here...
                               ; abstract grammar representation of (ref 1)
                               (term (to-abstract (ref 1)))
                               )
                   
                   ))
   
   (where any_2 ,(if (not (redex-match ext-lang
                                       (< e ... >)
                                       (term any_1)))
                     ; if the parsing succeeded, return the code obtained
                     ; wrapped into a vararg function
                     (append (term (function $loaded (<<<)))
                             (if (not (equal? (term v_3) (term nil)))
                                 ; received new value for the global
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
                     ; otherwise, return the error message
                     (term any_1)))]

  ; default behavior for mode: "bt"
  [(δbasic load String v_1 v_2 v_3)
   any

   (where any (δbasic load String v_1 "b" v_3))

   (side-condition (is_fdef? (term any)))]

  [(δbasic load String v_1 v_2 v_3)
   (δbasic load String v_1 "t" v_3)
   ]
  
  ; Received a function from which we can obtain the string that represents the
  ; program
  [(δbasic load cid v_1 v_2 v_3)
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
  [(δbasic load v ...)
   (δbasic error String)
   
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
  [(δbasic loadfile String_1 v_2 v_3 θ)
   (δbasic load String_2 nil v_2 v_3)

   (where String_2 ,(with-handlers
                     ([exn:fail? (λ (e) (term nil))])
                      (file->string (term String_1))
                   ))]

  ; no file found
  [(δbasic loadfile String_1 v_2 v_3 θ)
   nil

   (where nil ,(with-handlers
                   ([exn:fail? (λ (e) (term nil))])
                 (file->string (term String_1))
                 ))]

  [(δbasic loadfile v_1 v_2 v_3 θ)
   (δbasic error String_1)

   (where String_1 (δbasic type v_1))

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
  [(δbasic next v_1 v_2 θ)
   (δbasic error String_2)
   
   (where String_1 (δbasic type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 to 'next' (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {the first argument is a pointer to a table}

  ; from the ref. manual: "The order in which the indices are enumerated is not
  ; specified, even for numeric indices"
  ; we use the order of occurrence of the fields in the int. rep. of the table 

  ; nil index, non empty table
  [(δbasic next objref nil (osp_1 ...
                        (objref ((\{ (\[ v_1 \] = v_2) field ... \}) any ...))
                        osp_2 ...))
   (< v_1 v_2 >)]
  
  ; nil index, empty table
  [(δbasic next objref nil θ)
   (< nil >)]
  
  ; not the last index
  [(δbasic next objref v_1 (osp_1 ...
                        (objref
                         ((\{ field_1 ... (\[ v_2 \] = v_3) (\[ v_4 \] = v_5)
                              field_2 ... \})
                          any ...))
                        osp_2 ...))
   (< v_4 v_5 >)
   
   (side-condition (is_true? (term (δbasic == v_1 v_2))))
   ]
  
  ; Last index
  [(δbasic next objref v_1 (osp_1 ...
                        (objref
                         ((\{ field_1 ... (\[ v_2 \] = v_3) \}) any ...))
                        osp_2 ...))
   (< nil >)
   
   (side-condition (is_true? (term (δbasic == v_1 v_2))))
   ]
  
  ; Invalid key.
  [(δbasic next objref v θ)
   (δbasic error "invalid key to 'next'")]
  
  
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
  [(δbasic pairs objref θ)
   ((function $pairsCustomIter ()
              (local v1 v2 v3 = (any (objref))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (where any (indexMetaTable objref "__pairs" θ))
   
   (side-condition (not (is_nil? (term any))))]

  ; Default iterator: next
  [(δbasic pairs objref θ)
   (< (function $next (table index)
                (return ($builtIn next (table index)))
                end) objref nil >)
   
   (where nil (indexMetaTable objref "__pairs" θ))]
  
  [(δbasic pairs v θ)
   (δbasic error String_2)
   
   (where String_1 (δbasic type v))
   
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
  
  [(δbasic pcall v_1 v_2 ...)
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
  [(δbasic print v_1 ... ((refStdout String_1) vsp ...) θ)
   ((refStdout String_3) vsp ...)

   ; concat v_1 ... into a single string
   (where String_2 ,(foldl (λ (str accum)
                             (string-append accum
                                            (term (δbasic tostring ,str θ))
                                            )
                            )
                           (term String_1)
                           (term (v_1 ...))))

   (where String_3, (string-append (term String_2)
                                   "\n"))
   ]

  ; nothing has been written to stdout
  [(δbasic print v_1 ... (vsp ...) θ)
   ((refStdout String_2) vsp ...)
   
   (where String_1 ,(foldl (λ (str accum)
                             (string-append accum
                                            (term (δbasic tostring ,str θ))
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
  
  [(δbasic rawequal v_1 v_2)
   (δbasic == v_1 v_2)]
  
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
  [(δbasic rawget v_1 v_2 θ)
   (δbasic error String_2)
   
   (where String_1 (δbasic type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append 
                     "bad argument #1 (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {objref points to a table}
  [(δbasic rawget objref_1 v_1
              (osp_1 ...
               (objref_1 ((\{ field_1 ... (\[ v_2 \] = v_3) field_2 ... \})
                          any ...))
               osp_2 ...))
   v_3
   
   (side-condition (equal? (term (δbasic == v_1 v_2))
                           'true))]
  
  ; {v isn't a key of the table pointed by objref}
  [(δbasic rawget objref v θ)
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
  [(δbasic rawlen v θ)
   (δbasic error "bad argument #1 to 'rawlen'(table or string expected)")
   
   (where string (δbasic type v))
   
   (side-condition (not (or (equal? (term string)
                                    "table")
                            
                            (equal? (term string)
                                    "string"))))]
  
  [(δbasic rawlen String θ)
   (δbasic \# String)]
  
  [(δbasic rawlen objref_1 (osp_1 ...
                        (objref_1 (tableconstructor any ...))
                        osp_2 ...))
   (δbasic \# tableconstructor)]
  
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
  [(δbasic rawset v_1 v_2 v_3 θ)
   (θ (δbasic error String_2))
   
   (where String_1 (δbasic type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 (table expected, got "
                     (term String_1)
                     ")"))]

  ; {(δbasic type v_1) == "table"}
  ; Bad argument #2 to 'rawset'
  [(δbasic rawset v_1 nil v_3 θ)
   (θ (δbasic error String_2))
   
   (where String_2 "table index is nil")]
  
  [(δbasic rawset v_1 +nan.0 v_3 θ)
   (θ (δbasic error String_2))
   
   (where String_2 "table index is NaN")]
  
  ; Delete field
  [(δbasic rawset tid v_1 nil θ)
   ((osp_1 ...
     (tid ((\{ field_1 ... field_2 ... \}) any ...))
     osp_2 ...) tid)

   (where (side-condition
           (tid v_1 (osp_1 ...
                     (tid ((\{ field_1 ... (\[ v_2 \] = v_3)
                               field_2 ... \})
                           any ...))
                     osp_2 ...))
           (equal? (term (δbasic == v_1 v_2))
                   'true))
          (tid v_1 θ))
   ]
  
  ; v_2 != nil
  [(δbasic rawset objref v_1 v_2 (osp_1 ...
                              (objref ((\{ field_1 ... (\[ v_3 \] = v_4)
                                           field_2
                                           ... \})
                                       any ...))
                              osp_2 ...))
   ((osp_1 ...
     (objref ((\{ field_1 ... (\[ v_3 \] = v_2) field_2 ... \}) any ...))
     osp_2 ...) objref)

   (side-condition (equal? (term (δbasic == v_1 v_3))
                           'true))
   ]
  
  ; Add a new field 
  [(δbasic rawset objref v_1 v_2 (osp_1 ...
                              (objref ((\{ field ... \}) any ...))
                              osp_2 ...))

   ((osp_1 ...
     (objref ((\{ (\[ v_1 \] = v_2) field ... \}) any ...))
     osp_2 ...) objref)

   (side-condition (not (equal? (term v_2)
                                (term nil))))]

  [(δbasic rawset objref v_1 nil (osp_1 ...
                              (objref ((\{ field ... \}) any ...))
                              osp_2 ...))

   ((osp_1 ... (objref ((\{ field ... \}) any ...)) osp_2 ...) objref)]

  ; Wrong arguments
  [(δbasic rawset v ... nil θ)
   (θ (δbasic error "rawset: missing arguments"))

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
  ; index out of range
  [(δbasic select Number_1 v ...)
   (δbasic error "bad argument #1 to 'select' (index out of range)")
   
   (where Number_2 ,(* -1 (length (term (v ...)))))

   (where Number_3 ,(exact-floor (term Number_1)))
   
   (side-condition (or (< (term Number_3) (term Number_2))
                       (= (term Number_3) 0)))]
  
  ; positive index in the range [1;(length (v ...))] 
  [(δbasic select Number_1 v ...)
   (< v_2 ... >)
   
   (where Number_2 ,(exact-floor (term Number_1)))
   
   (side-condition (and (<= 1 (term Number_2))
                        (<= (term Number_2) (length (term (v ...))))))

   (where (v_2 ...) ,(list-tail (term (v ...)) (- (term Number_2) 1)))]
  
  ; positive NUmber_1 > (length (sv ...))
  [(δbasic select Number_1 v ...)
   (< >)
   
   (where Number_2 ,(exact-floor (term Number_1)))
   
   (side-condition (> (term Number_2) (length (term (v ...)))))]
  
  ; Negative index
  [(δbasic select Number_1 v_1 ...)
   (< v_2 ... >)

   (where Number_2 ,(length (term (v_1 ...))))
   (where Number_3 ,(* -1 (term Number_2)))
   (where Number_4 ,(exact-floor (term Number_1)))
   
   (side-condition (and (<= (term Number_3) (term Number_4))
                        (<= (term Number_4) -1)))
   
   ; substract Number_1 to the length of v_1 ...
   (where Number_5 ,(+ (term Number_2)
                       (term Number_4)))

   (where (v_2 ...) ,(list-tail (term (v_1 ...)) (term Number_5)))]
  
  ; Obtain the total number of actual arguments received.
  [(δbasic select "#" v ...)
   (< any >)
   
   (where any ,(length (term (v ...))))]
  
  [(δbasic select '#' v ...)
   (< any >)
   
   (where any ,(length (term (v ...))))]
  
  ; Default case
  [(δbasic select v_1 v ...)
   (δbasic error "bad argument #1 to 'select' (number expected)")]
  
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
  [(δbasic setmetatable v_1 v_2 θ)
   (θ (δbasic error String_2))
   
   (where String_1 (δbasic type v_1))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 to 'setmetatable' (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {objref is a pointer to a table}
  ; Bad argument #2.
  [(δbasic setmetatable objref v θ)
   (θ (δbasic error "bad argument #2 to 'setmetatable' (nil or table expected)"))
   
   (where String_1 (δbasic type v))
   
   (side-condition (not (or (equal? (term String_1)
                                    "table")
                            
                            (equal? (term String_1)
                                    "nil"))))]
  
  ; {v points to a table or is nil}
  
  ; Protected meta-table
  [(δbasic setmetatable objref v θ)
   (θ (δbasic error "cannot change a protected metatable"))
   
   (side-condition (term (protectedMetaTable? objref θ)))]
  
  ; Non protected meta-table
  [(δbasic setmetatable tid v (osp_1 ...
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
  [(δbasic tonumber Number v)
   (δbasic tonumber String v)

   ; since Number could be in a non-decimal base v, it is easier to coerce it
   ; to string, and apply the same algorithm of conversion over it
   (where String (δbasic tostring Number ()))
   ]

  ; decimal number, no base specified
  [(δbasic tonumber String nil)
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
  [(δbasic tonumber v nil)
   nil]

  ; base out of range
  [(δbasic tonumber v 1)
   (δbasic error String)

   (where String ,(string-append
                   "bad argument #2 to 'tonumber' (base out of range)"))]
  
  ; when called with a base (Number), then the first argument should be a string
  ; to be interpreted as an integer numeral in that base
  [(δbasic tonumber String Number)
   (convert_string String Number)]

  ; {v_2 ≠ nil ∧ (v_1 ∉ String ∨ v_1 cannot be coerced to a number)}
  [(δbasic tonumber v_1 v_2)
   (δbasic error String)

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
  [(δbasic tostring objref θ)
   (any (objref))

   ; index metatable of objref
   (where any (indexMetaTable objref "__tostring" θ))

   (side-condition (not (is_nil? (term any))))]
  
  ; v has an associated metatable, with field ("__tostring" = any)
  [(δbasic tostring v θ)
   (any (v))

   (side-condition (not (is_tid? (term v))))
   
   (where objref (getMetaTableRef v))
   
   (side-condition (term (refBelongsToTheta? objref θ)))

   ; directly index metatable objref
   (where any (δbasic rawget objref "__tostring" θ))

   (side-condition (not (is_nil? (term any))))]
  
  ; {meta-table of value's type is not set ∨ meta-table does not have
  ; __tostring field ∨ v ∈ objref}
  
  ; implement custom conversions to string
  ; simple way of avoiding exception check, when using inexact->exact, below
  [(δbasic tostring +nan.0 θ)
   "nan"]

  [(δbasic tostring +inf.0 θ)
   "inf"]

  [(δbasic tostring -inf.0 θ)
   "-inf"]
  
  ; to implement the behaviour of Lua's coercion: 1.0 .. 1.0 = "11"
  [(δbasic tostring Number θ)
   ; {Number ∉ {nan, inf}}
   ,(~a (inexact->exact (term Number)))

   (side-condition (= (floor (term Number))
                      (term Number)))]
  
  [(δbasic tostring Number θ)
   ,(~a (term Number))

   (side-condition (not (= (floor (term Number))
                           (term Number))))]
  
  [(δbasic tostring v θ)
   String_2

   ; v is a table or closure id
   (side-condition (or (is_tid? (term v))
                       (is_cid? (term v))))
   
   (where String_1 (δbasic type v))
   
   (where String_2 ,(string-append (term String_1) ": "
                                   (~a (term v))))]

  ; {v ∉ tid ∪ cid}
  ; default case: racket/format conversion
  [(δbasic tostring v θ)
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

  [(δbasic type Number )
   "number"]
  
  [(δbasic type nil)
   "nil"]
  
  [(δbasic type Boolean)
   "boolean"]
  
  [(δbasic type String)
   "string"]
  
  [(δbasic type cid)
   "function"]
  
  ; Default case
  [(δbasic type objref)
   "table"]
  
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
  [(δbasic xpcall v_1 v_2 v_3 ...)
   ((v_1 (v_3 ...)) ProtectedMode v_2)]

  ; to capture the "no value" error for every builtinserv 
  [(δbasic builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))]
  
  ; services that don't modify theta
  [(δbasic builtinserv v ... θ)
   (δbasic error any)

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
                                  rawlen))))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]

  ; services that modify theta
  [(δbasic builtinserv v ... θ)
   (θ (δbasic error any))
   
   (where any ,(string-append (symbol->string (term builtinserv))
                              " got no value"))
   ]
  )

(provide δbasic)

; To convert booleans values in racket to boolean values in our language 
(define-metafunction ext-lang
  
  [(toBool #t)
   true]
  
  [(toBool any)
   false])

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
   
   (where v_2 (δbasic rawget objref_2 v_1 (osp_1 ...
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
   
   (where v_3 (δbasic rawget objref v_2 θ))])

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
