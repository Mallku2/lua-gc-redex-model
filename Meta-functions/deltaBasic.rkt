#lang racket
(require redex
         math/flonum ; operations over flonums
         "../grammar.rkt"
         "./objStoreMetaFunctions.rkt"
         "./grammarMetaFunctions.rkt"
         "./coercion.rkt"
         "./gc.rkt"
         "../Desugar/parser.rkt"
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
   ,(fl+ (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic - Number_1 Number_2)
   ,(fl- (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic * Number_1 Number_2)
   ,(fl* (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
 
  [(δbasic / Number_1 Number_2)
   ,(fl/ (term Number_3) (term Number_4))

   ; to guarantee ieee 754 behavior, we apply a step of conversion to flonums;
   ; hence, for example, a division (/ 1 0) results in +inf.0, as in Lua,
   ; instead of raising an exception
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic ^ Number_1 Number_2)
   ,(flexpt (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δbasic % Number_1 Number_2)
   ;from ref. manual: "it is the remainder of a division that rounds the
   ; quotient towards minus infinity": a - math.floor(a/b)*b
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

  [(δbasic - Number_1)
   ,(fl- (term Number_2))
   
   (where Number_2 ,(real->double-flonum (term Number_1)))]
  
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
  
  ; Table length: just the max. positive numeric key
  [(δbasic \# (\{ field ... \}))
   any

   ; extract positive numeric keys
   (where ((\[ Number_1 \] = v_1) (\[ Number_2 \] = v_2) ...)
    ,(filter (lambda (field)
                     (redex-match? ext-lang
                                   (side-condition (|[| v_2 |]| = v_3)
                                                   (and (is_number? (term v_2))
                                                        (positive-integer? (term v_2))))
                                   (term ,field)))
             (term (field ...))))
   
   (where any ,(argmax (λ (number) number) (term (Number_1 Number_2 ...))))]
  
  ; Table doesn't have positive numeric keys
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
  ; assert: weird behavior
  [(δbasic assert)
   (δbasic error "assertion failed!")]

  ; condition evaluates to true
  [(δbasic assert v_1 v_2 ...)
   (< v_1 v_2 ... >)

   (side-condition (not (is_false_cond? (term v_1))))]

  ; default error message
  [(δbasic assert v_1)
   (δbasic assert v_1 "assertion failed!")]

  [(δbasic assert v_1 nil v ...)
   (δbasic assert v_1 "assertion failed!")]

  ; {is_false_cond? v_1}
  ; {v_2 is a proper error message}
  [(δbasic assert v_1 v_2 v_3 ...)
   (δbasic error v_2)]

  
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
  ; correction of parameters
  [(δbasic error)
   (δbasic error nil 1)]
  
  [(δbasic error v_1)
   (δbasic error v_1 1)]

  [(δbasic error v_1 nil v ...)
   (δbasic error v_1 1)]

  [(δbasic error v_1 v_2 v_3 v_4 ...)
   (δbasic error v_1 v_2)]

  ; coercion
  [(δbasic error v_1 String)
   (δbasic error v_1 Number)

   (where Number (δbasic tonumber v_1 nil))]

  ; TODO: model level
  [(δbasic error v Number)
   ($err v)]
  
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
  ;
  ; discarding surplus parameters
  [(δbasic getmetatable v_1 v_2 v_3 ... θ)
   (δbasic getmetatable v_1 θ)]
  
  ; table value has a meta-table, which has a "__metatable" key.
  [(δbasic getmetatable tid_1 (osp_1 ...
                               (tid_1 (tableconstructor tid_3 any_1))
                               osp_2 ...))
   v
   
   (where (osp_3 ...
           (tid_3 ((\{ field_1 ... (\[ "__metatable" \] = v) field_2 ... \})
                   any_2 ...))
           osp_4 ...)

          (osp_1 ...
           (tid_1 (tableconstructor tid_3 any_1))
           osp_2 ...))

   (side-condition (not (is_nil? (term v))))]

  ; table value doesn't have a protected meta-table
  [(δbasic getmetatable tid (osp_1 ...
                             (tid (tableconstructor v any))
                             osp_2 ...))
   v]

  
  ; the value isn't a table. It has a meta-table, which has
  ; a "__metatable" key,
  [(δbasic getmetatable any_1 (osp_1 ...
                               (tid_2 ((\{ field_1 ...
                                           (\[ "__metatable" \] = v)
                                           field_2 ... \})
                                       any_2 ...))
                               osp_2 ...))
   v
   
   (where tid_2 (getMetaTableRef any_1))]
  
  ; the value isn't a table. It has a meta-table, which has not a "__metatable"
  ; key
  [(δbasic getmetatable any_1 (osp_1 ...
                               (tid_2 any_2)
                               osp_2 ...))
   tid_2
   
   (where tid_2 (getMetaTableRef any_1))]

  ; the value isn't a table and its type has not a meta-table set.
  [(δbasic getmetatable v θ)
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
  ; discarding surplus parameters
  [(δbasic ipairs tid v_1 v_2 ... θ)
   (δbasic ipairs tid θ)]
  
  ; custom iterator, provided by the metatable
  [(δbasic ipairs tid θ)
   ((function $IpairsCustomIter ()
              (local v1 v2 v3 = (v (tid))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (where v (indexMetaTable tid "__ipairs" θ))
   
   (side-condition (not (is_nil? (term v))))]

  ; default iterator
  [(δbasic ipairs tid θ)
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
                end) tid 0 >)
   
   (where nil (indexMetaTable tid "__ipairs" θ))]
  
  
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
  ; default values for source, mode, env
  [(δbasic load v)
   (δbasic load v String "bt" (objr ,objStoreFirstLocation))

   (where String ,(if (is_string? (term v))
                      (term v)
                      "=(load)"))]

  [(δbasic load v_1 v_2)
   (δbasic load v_1 v_2  "bt" (objr ,objStoreFirstLocation))]

  [(δbasic load v_1 v_2 v_3)
   (δbasic load v_1 v_2 v_3 (objr ,objStoreFirstLocation))]

  ; final check of parameters
  [(δbasic load v_1 v_2 v_3 v_4 v_8 ...)
   (δbasic load v_1 v_5 v_6 v_7)

   (side-condition (or (is_nil? (term v_2))
                       (is_nil? (term v_3))
                       (is_nil? (term v_4))))
   
   (where v_5 ,(if (is_string? (term v_2))
                   (term v_2)
                   (if (is_string? (term v_1))
                       (term v_1)
                       "=(load)")))

   (where v_6 ,(if (is_string? (term v_3))
                   (term v_3)
                   "bt"))

   (where v_7 ,(if (is_tid? (term v_4))
                   (term v_4)
                   ; tid of the global environment
                   (term (objr ,objStoreFirstLocation))))]
  
  [(δbasic load String_1 v_1 "b" v_2)
   t

   ; String_1 should be a string (flattened) representation of a function
   ; (obtained through string.dump). We reuse Racket's reader to
   ; try to parse it and obtain the corresponding redex term.
   (where t ,(with-handlers
                 ([exn:fail?
                   ; the string cannot be parsed
                   (λ (e) (term (< nil
                                   "attempt to load a text chunk (mode is 'b')"
                                   >)))])
                   
               (read (open-input-string (term String_1)))
               ))]
 
  ; Lua program expressed into a string, either syntactically correct or not 
  [(δbasic load String v_1 "t" v_2)
   any_2
   
   (where any_1 ,(with-handlers
                     ([exn:fail?
                       ; the string cannot be parsed
                       (λ (e) (append (term (< nil ))
                                      (list (if (is_nil? (term v_1))
                                                
                                                (string-append "[string "
                                                               (term String)
                                                               "]")
                                                           
                                                (string-append "[string "
                                                               (term v_1)
                                                               "]")))
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
                             (if (not (equal? (term v_2) (term nil)))
                                 ; received new value for the global
                                 ; environment
                                 (list (term
                                        (local $old_env $ret = (ref 1) nil
                                          in
                                          (((ref 1) = v_2)
                                           ; first, we need to evaluate any_1 into
                                           ; a modified environment;
                                           ; note that any_1 could return an
                                           ; arbitrary quantity of values: we
                                           ; put returned values into a table
                                           ($ret = (\{ ((\( (function $aux (<<<)
                                                                  any_1
                                                                  end) \))
                                                        (<<<)) \}))
                                           ; later, we need to restore
                                           ; the environment
                                           ((ref 1) = $old_env)
                                           ; finally, we unpack the values in
                                           ; $ret
                                           (return ($builtIn table.unpack ($ret))))
                                          end)))
                                 
                                 (list (term any_1)))
                             
                             (term (end)))
                     ; otherwise, return the error message
                     (term any_1)))]

  ; default behavior for mode: "bt"
  [(δbasic load String v_1 "bt" v_2)
   any

   (where any (δbasic load String v_1 "b" v_2))

   (side-condition (is_fdef? (term any)))]

  [(δbasic load String v_1 "bt" v_2)
   (δbasic load String v_1 "t" v_2)]
  
  ; received a function from which we can obtain the string that represents the
  ; program
  [(δbasic load cid v_1 v_2 v_3)
   any_2

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
                              
                              (return ($builtIn load (program v_1 v_2 v_3))))
                             
                             end)
                           end) ()))]

  ; this service does not inform about errors processing String_1
  [(δbasic load String_1 String_2 String_3 tid)
   nil]

  
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
  ; TODO: include reading from standard input
  ; default values
  [(δbasic loadfile String θ)
   (δbasic loadfile String "bt" (objr ,objStoreFirstLocation) θ)]

  [(δbasic loadfile String v θ)
   (δbasic loadfile String v (objr ,objStoreFirstLocation) θ)]

  ; last check
  [(δbasic loadfile String_1 v_1 v_2 v_3 ... θ)
   (δbasic loadfile String_1 String_2 tid θ)

   (side-condition (or (is_nil? (term v_1))
                       (is_nil? (term v_2))))

   (where String_2 ,(if (is_string? (term v_1))
                        (term v_1)
                        "bt"))

   (where tid ,(if (is_tid? (term v_2))
                   (term v_2)
                   (term (objr ,objStoreFirstLocation))))]
   
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
  ; default values
  [(δbasic next tid θ)
   (δbasic next tid nil θ)]

  [(δbasic next tid v_1 v_2 v_3 ... θ)
   (δbasic next tid v_1 θ)]

  ; from the ref. manual: "The order in which the indices are enumerated is not
  ; specified, even for numeric indices"
  ; we use the order of occurrence of the fields in the int. rep. of the table 

  ; nil index, non empty table
  [(δbasic next tid nil (osp_1 ...
                         (tid ((\{ (\[ v_1 \] = v_2) field ... \}) any ...))
                         osp_2 ...))
   (< v_1 v_2 >)]
  
  ; nil index, empty table
  [(δbasic next tid nil θ)
   (< nil >)]
  
  ; not the last index
  [(δbasic next tid v_1 (osp_1 ...
                         (tid
                          ((\{ field_1 ... (\[ v_2 \] = v_3) (\[ v_4 \] = v_5)
                               field_2 ... \})
                           any ...))
                         osp_2 ...))
   (< v_4 v_5 >)
   
   (where true (δbasic == v_1 v_2))]
  
  ; Last index
  [(δbasic next tid v_1 (osp_1 ...
                         (tid
                          ((\{ field_1 ... (\[ v_2 \] = v_3) \}) any ...))
                         osp_2 ...))
   (< nil >)
   
   (where true (δbasic == v_1 v_2))]
  
  ; invalid key.
  [(δbasic next tid v θ)
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
  [(δbasic pairs tid v_1 v_2 ... θ)
   (δbasic pairs tid θ)]
  
  ; custom iterator, provided by the metatable
  [(δbasic pairs tid θ)
   ((function $pairsCustomIter ()
              (local v1 v2 v3 = (v (tid))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (where v (indexMetaTable tid "__pairs" θ))
   
   (side-condition (not (is_nil? (term v))))]

  ; Default iterator: next
  [(δbasic pairs tid θ)
   (< (function $next (<<<)
                (return ($builtIn next (<<<)))
                end) tid nil >)
   
   (where nil (indexMetaTable tid "__pairs" θ))]
  
;  [(δbasic pairs v θ)
;   (δbasic error String_2)
;   
;   (where String_1 (δbasic type v))
;   
;   (where String_2 ,(string-append "bad argument #1 (table expected, got "
;                                   (term String_1)
;                                   ")"))]
  
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
                     (function $handler (errMsg)
                               (return errMsg) end)
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

  ; hack to implement a simple instrumentation tool.
  ; nothing has been written to stdout
  [(δbasic print v_1 ... ((r v) ...) θ)
   ; create stdout file
   (δbasic print v_1 ... ((refStdout "") (r v) ...) θ)]
  
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

   (side-condition (println (term String_3)))]

  
  
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
  ;
  ; discarding surplus parameters: defined here for clarity purposes                         
  [(δbasic rawequal v_1 v_2 v_3 v_4 ...)
   (δbasic rawequal v_1 v_2)]
  
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
  [(δbasic rawget tid v_1 v_2 v_3 ... θ)
   (δbasic rawget tid v_1 θ)]
  
  ; {tid points to a table}
  [(δbasic rawget tid_1 v_1
           (osp_1 ...
            (tid_1 ((\{ field_1 ... (\[ v_2 \] = v_3) field_2 ... \})
                    any ...))
            osp_2 ...))
   v_3
   
   (side-condition (equal? (term (δbasic == v_1 v_2))
                           'true))]
  
  ; {v isn't a key of the table pointed by tid}
  [(δbasic rawget tid v θ)
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
  [(δbasic rawlen v_1 v_2 v_3 ... θ)
   (δbasic rawlen v_1 θ)]
  
  [(δbasic rawlen String θ)
   (δbasic \# String)]
  
  [(δbasic rawlen tid (osp_1 ...
                       (tid (tableconstructor any ...))
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
  [(δbasic rawset tid v_1 v_2 v_3 v_4 ... θ)
   (δbasic rawset tid v_1 v_2 θ)]

  ; special erroneous cases
  ; bad argument #2 to 'rawset'
  [(δbasic rawset tid nil v θ)
   (θ (δbasic error String_2))
   
   (where String_2 "table index is nil")]
  
  [(δbasic rawset tid +nan.0 v θ)
   (θ (δbasic error String_2))
   
   (where String_2 "table index is NaN")]

  ; {v_1 ∉ {nil, Nan}}
  ; delete field
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
          (tid v_1 θ))]

  ; special case: v ∉ dom(θ(tid))
  [(δbasic rawset tid v nil θ)
   (θ tid)]
  
  ; v_2 != nil
  ; field update
  [(δbasic rawset tid v_1 v_2 (osp_1 ...
                               (tid ((\{ field_1 ... (\[ v_3 \] = v_4)
                                         field_2
                                         ... \})
                                     any ...))
                               osp_2 ...))
   ((osp_1 ...
     (tid ((\{ field_1 ... (\[ v_3 \] = v_2) field_2 ... \}) any ...))
     osp_2 ...) tid)

   (where true (δbasic == v_1 v_3))]
  
  ; add a new field 
  [(δbasic rawset tid v_1 v_2 (osp_1 ...
                               (tid ((\{ field ... \}) any ...))
                               osp_2 ...))

   ((osp_1 ...
     (tid ((\{ (\[ v_1 \] = v_2) field ... \}) any ...))
     osp_2 ...) tid)

   (side-condition (not (equal? (term v_2)
                                (term nil))))]
  
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
  ; coercion
  [(δbasic select String v ...)
   (δbasic select Number v ...)

   (where Number (δbasic tonumber String nil))]
  
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
  
  ; positive NUmber_1 > (length (v ...))
  [(δbasic select Number_1 v ...)
   (< >)
   
   (where Number_2 ,(exact-floor (term Number_1)))
   
   (side-condition (> (term Number_2) (length (term (v ...)))))]
  
  ; negative index
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
  
  ; obtain the total number of actual arguments received.
  [(δbasic select "#" v ...)
   (< Number >)
   
   (where Number ,(length (term (v ...))))]
  
  [(δbasic select '#' v ...)
   (< Number >)
   
   (where Number ,(length (term (v ...))))]
  
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
  [(δbasic setmetatable tid v_1 v_2 v_3 ... θ)
   (δbasic setmetatable tid v_1 θ)

   (side-condition (or (is_tid? (term v_1))
                       (is_nil? (term v_1))))]
 
  ; {v ∈ {tid, nil}}
  ; protected meta-table
  [(δbasic setmetatable tid v θ)
   (θ (δbasic error "cannot change a protected metatable"))
   
   (side-condition (term (protectedMetaTable? tid θ)))

   (side-condition (or (is_tid? (term v))
                       (is_nil? (term v))))]
  
  ; non protected meta-table
  [(δbasic setmetatable tid v_1 (osp_1 ...
                                 (tid (evaluatedtable v_2 pos_1))
                                 osp_2 ...))
   ((osp_1 ...
     (tid (evaluatedtable v_1 pos_2))
     osp_2 ...) tid)

   (side-condition (or (is_tid? (term v_1))
                       (is_nil? (term v_1))))
   
   ; if metatable has a __gc field, set tid for finalization
   (where pos_2 (setFin tid v_1 (osp_1 ...
                                 (tid (evaluatedtable v_2 pos_1))
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
  [(δbasic tonumber v)
   (δbasic tonumber v nil)]

  [(δbasic tonumber v_1 v_2 v_3 v_4 ...)
   (δbasic tonumber v_1 v_2)]
  
  [(δbasic tonumber Number v)
   (δbasic tonumber String v)

   ; since Number could be in a non-decimal base v, it is easier to coerce it
   ; to string, and apply the same algorithm of conversion over it
   (where String (δbasic tostring Number ()))]

  ; decimal number, no base specified
  [(δbasic tonumber String nil)
   v
   
   ; though the manual does not specify this, in this case tonumber
   ; converts String following the rules of the lexer, as said by the semantics;
   ; however, lexer alone will not suffice: for example, in case of malformed
   ; strings beginning with a correct string representation of numbers.
   (where v ,(with-handlers ([exn:fail?
                                           (λ (e) nil)])
                            ((λ ()
                               ; to use the parser, we need to feed it with an
                               ; statement...
                               ; NOTE: we append String directly. Then, the
                               ; conversion to a number is done by the lexer/parser.
                               (number-parse-this (term String))))))]

;  [(δbasic tonumber String nil)
;   Number
;   
;   ; though the manual does not specify this, in this case tonumber
;   ; converts String following the rules of the lexer, as said by the semantics;
;   ; however, lexer alone will not suffice: for example, in case of malformed
;   ; strings beginning with a correct string representation of numbers.
;   (where (any = (0.0 + Number)) ,(with-handlers ([exn:fail?
;                                           (λ (e) #f)])
;                            ((λ ()
;                               ; to use the parser, we need to feed it with an
;                               ; statement...
;                               ; NOTE: we append String directly. Then, the
;                               ; conversion to a number is done by the lexer/parser.
;                               (parse-this (string-append "_ENV = 0" (term String))
;                                           #f
;                                           (void))))))]

;  [(δbasic tonumber String nil)
;   (δbasic - Number)
;   
;   ; though the manual does not specify this, in this case tonumber
;   ; converts String following the rules of the lexer, as said by the semantics;
;   ; however, lexer alone will not suffice: for example, in case of malformed
;   ; strings beginning with a correct string representation of numbers.
;   (where (any = (- Number)) ,(with-handlers ([exn:fail?
;                                           (λ (e) #f)])
;                            ((λ ()
;                               ; to use the parser, we need to feed it with an
;                               ; statement...
;                               ; NOTE: we append String directly. Then, the
;                               ; conversion to a number is done by the lexer/parser.
;                               (parse-this (string-append "_ENV = " (term String))
;                                           #f
;                                           (void))))))]

  ; {v ∉ String}
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

  [(δbasic tostring v_1 v_2 v_3 ... θ)
   (δbasic tostring v_1 θ)]
  
  ; table tid has an associated metatable, with field ("__tostring" = any)
  [(δbasic tostring tid θ)
   (any (tid))

   ; index metatable of tid
   (where any (indexMetaTable tid "__tostring" θ))

   (side-condition (not (is_nil? (term any))))]
  
  ; v has an associated metatable, with field ("__tostring" = any)
  [(δbasic tostring v θ)
   (any (v))

   (side-condition (not (is_tid? (term v))))
   
   (where tid (getMetaTableRef v))
   
   (side-condition (term (refBelongsToTheta? tid θ)))

   ; directly index metatable tid
   (where any (δbasic rawget tid "__tostring" θ))

   (side-condition (not (is_nil? (term any))))]
  
  ; {meta-table of value's type is not set ∨ meta-table does not have
  ; __tostring field ∨ v ∈ tid}
  
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
  
  [(δbasic type v_1 v_2 v_3 ...)
   (δbasic type v_1)]

  [(δbasic type Number)
   "number"]
  
  [(δbasic type nil)
   "nil"]
  
  [(δbasic type Boolean)
   "boolean"]
  
  [(δbasic type String)
   "string"]
  
  [(δbasic type cid)
   "function"]
  
  [(δbasic type tid)
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
   ((v_1 (v_3 ...)) ProtMD v_2)]
  
  
  
  ;                                                        
  ;                                                        
  ;                                                        
  ;                                                        
  ;                                                        
  ;                                                        
  ;     ;;;      ; ;;;    ; ;;;    ;;;      ; ;;;   ;;;;;  
  ;    ;   ;     ;;   ;   ;;   ;  ;   ;     ;;   ; ;     ; 
  ;   ;     ;    ;        ;      ;     ;    ;      ;       
  ;   ;     ;    ;        ;      ;     ;    ;      ;;;;    
  ;   ;;;;;;;    ;        ;      ;     ;    ;          ;;; 
  ;   ;          ;        ;      ;     ;    ;            ; 
  ;    ;    ;    ;        ;       ;   ;     ;      ;     ; 
  ;     ;;;;     ;        ;        ;;;      ;       ;;;;;  
  ;                                                        
  ;                                                        
  ;                                                        
  ;                                                        

  ; to capture the "no value" error for every builtinserv 
  [(δbasic builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))]
  
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
   
   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))
   ]

  ; services that modify theta
  [(δbasic builtinserv v ... θ)
   (θ (δbasic error any))
   
   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))
   ]
  )

(provide δbasic)

; To convert booleans values in racket to boolean values in our language 
(define-metafunction ext-lang
  
  [(toBool #t)
   true]
  
  [(toBool any)
   false])


;                                                                                                                                         
;                                                                                                                                         
;                                                                                                                                         
;                                                                  ;       ;;;;                                                           
;                       ;                          ;               ;          ;                                                           
;                       ;                          ;               ;          ;                                                           
;   ;;;;;;     ;;;    ;;;;;;     ;;;;            ;;;;;;     ;;;;   ; ;;;      ;        ;;;     ;;;;;              ;;;;   ;     ;  ;;   ;; 
;   ;  ;  ;   ;   ;     ;       ;    ;             ;       ;    ;  ;;   ;     ;       ;   ;   ;     ;            ;    ;  ;     ;   ;   ;  
;   ;  ;  ;  ;     ;    ;            ;             ;            ;  ;     ;    ;      ;     ;  ;                       ;  ;     ;    ; ;   
;   ;  ;  ;  ;     ;    ;       ;;;;;;    ;;;;     ;       ;;;;;;  ;     ;    ;      ;     ;  ;;;;               ;;;;;;  ;     ;     ;    
;   ;  ;  ;  ;;;;;;;    ;      ;;    ;             ;      ;;    ;  ;     ;    ;      ;;;;;;;      ;;;           ;;    ;  ;     ;     ;    
;   ;  ;  ;  ;          ;      ;     ;             ;      ;     ;  ;     ;    ;      ;              ;           ;     ;  ;     ;    ; ;   
;   ;  ;  ;   ;    ;    ;      ;    ;;             ;      ;    ;;  ;;   ;     ;       ;    ;  ;     ;           ;    ;;  ;;   ;;   ;   ;  
;   ;  ;  ;    ;;;;      ;;;    ;;;; ;              ;;;    ;;;; ;  ; ;;;       ;;;     ;;;;    ;;;;;             ;;;; ;   ;;;; ;  ;;   ;; 
;                                                                                                                                         
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

; returns the meta-table tid of a given value, if set, or nil, otherwise
(define-metafunction ext-lang
  getMetaTable : v θ -> v

  [(getMetaTable tid (osp_1 ...
                      (tid (evaluatedtable v any))
                      osp_2 ...))
   v]

  ; {v ∉ tid}
  [(getMetaTable v θ)
   tid
   
   (where tid (getMetaTableRef v))
   
   (side-condition (term (refBelongsToTheta? tid θ)))]

  ; {metatable of type(v) ∉ dom(θ)}
  [(getMetaTable _ _)
   nil])

(provide getMetaTable)
; meta-function that tries to get the meta-table of a given value and index it
; with a given key. If it doesn't succeed, it returns nil.
; PRE : {v_1 is the value whose meta-table we want to index and v_2 is the
;        key}
; ret = (indexMetaTable v_1 v_2 θ)
(define-metafunction ext-lang
  
  [(indexMetaTable v_1 v_2 θ)
   v_3

   (where tid (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid v_2 θ))]
  
  ; value does not have a meta-table set
  [(indexMetaTable _ _ _)
   nil])

(provide indexMetaTable)


(define-metafunction ext-lang
  ; protected meta-table
  [(protectedMetaTable? v θ)
   #t
   
   (side-condition (not (is_nil? (term (indexMetaTable v "__metatable" θ)))))]
  
  ; default case
  [(protectedMetaTable? v θ)
   #f])

(provide protectedMetaTable?)

; since theses defs. make use of δ, and δ uses them too, they cannot be put in
; an external module

; chooses a handler for a binary operation
; PRE : {v_1, v_2 are the operands and String is the string that serves as
;       key to index the meta-table
; any = (getBinHandler v_1 v_2 String θ)
; POS : {returns the tid and associated value of the indexed meta-table or nil,
;        if no value has a meta-table set with a proper value in field with
;        key String}
(define-metafunction ext-lang
  getBinHandler : v v String θ -> any
  
  [(getBinHandler v_1 v_2 String θ)
   (tid v_3)
   
   ; determine if v_1 has meta-table
   (where tid (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid String θ))

   (side-condition (not (is_false_cond? (term v_3))))]
  
  [(getBinHandler v_1 v_2 String θ)
   (tid v_3)
   
   ; determine if v_1 has meta-table
   (where tid (getMetaTable v_2 θ))
   (where v_3 (δbasic rawget tid String θ))
   
   (side-condition (not (is_false_cond? (term v_3))))]
  
  ; otherwise...
  [(getBinHandler v_1 v_2 String θ)
   nil])

(provide getBinHandler)

; chooses a handler for an unary operation
; PRE : {v is the operand and String is the string that serves as
;       key to index the meta-table
; ret = (getUnaryHandler v String θ)
; POS : {returns the value of v's meta-table indexed with key String (if
;        it belongs to the meta-table and the value is not nil or false), and
;        the tid of the indexed meta-table; or nil}
(define-metafunction ext-lang
  getUnaryHandler : v String θ -> any
  
  [(getUnaryHandler v_1 String θ)
   (tid v_2)
   
   ; determine if v has meta-table
   (where tid (getMetaTable v_1 θ))
   (where v_2 (δbasic rawget tid String θ))
   
   (side-condition (not (is_false_cond? (term v_2))))]
  
  ; otherwise
  [(getUnaryHandler v String θ)
   nil])

(provide getUnaryHandler)

; obtain a handler for an equality comparison, following the criterion defined
; in the procedure of the same name, in Lua's reference manual;
; it also returns the tid of every meta-table accessed
(define-metafunction ext-lang 
  
  ; the values compared are tables, with the same handler for the equality
  ; comparison
  [(getEqualHandler v_1 v_2 θ)
   (tid_3 ... v_3)
   
   (where true (δbasic == (δbasic type v_1) (δbasic type v_2)))

   (where true (δbasic == (δbasic type v_1) "table"))

   (where tid_1 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_1 "__eq" θ))
   ;(where any_1 (indexMetaTable v_1 "__eq" θ))

   (where tid_2 (getMetaTable v_2 θ))
   (where v_4 (δbasic rawget tid_2 "__eq" θ))
   ;(where any_2 (indexMetaTable v_2 "__eq" θ))

   ; compare handlers through Lua's equality
   (where true (δbasic == v_3 v_4))

   (where (tid_3 ...) ,(remove-duplicates (term (tid_1 tid_2))))]
  
  ; the values compared are tables, with the different handlers for the equality
  ; comparison
  [(getEqualHandler v_1 v_2 θ)
   nil
   
   (where true (δbasic == (δbasic type v_1) (δbasic type v_2)))

   (where true (δbasic == (δbasic type v_1) "table"))]
  
  ; the types of the values compared are different, or they are not tables
  [(getEqualHandler v_1 v_2 θ)
   nil])

(provide getEqualHandler)