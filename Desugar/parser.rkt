#lang racket

(require parser-tools/yacc
         "./lexer.rkt"
         "./phrases_constructors.rkt"
         "./scope.rkt")

; block
(define actual-block
  (new-empty-block))

(set! actual-block (new-block actual-block))

(set! actual-block
      (new-scope actual-block
                 (list (string->symbol "_ENV"))))


; replacement for _ENV, in case the parser is called in run-time
(define global-env-subst
  (id-name '_ENV))

; parser for Lua 5.2
(define lua-parser
  (parser
   
   ; Start symbol
   (start chunk)
   
   (end EOF)
   
   (error (lambda (tok-ok? tok-name tok-value)
            (begin
              ; the presence of an error while parsing could leave into the symbol
              ; table information that must be cleaned before a possible next
              ; call to the parser (eg. during run-time, while using load)
              (set! actual-block (new-empty-block))
              (error "Parser error. Token name:" tok-name))))

   ; Uncomment for debug
   ;(debug "log")
   
   ; Tokens declaration
   (tokens empty-tokens
           non-empty-tokens)
   
   ; Prec. declarations, to avoid shit-reduce conflicts
   (precs (nonassoc BEGINNING_WHILE)
          (nonassoc BEGINNING_DO_END)
          ; Taken from Lua's reference manual, 3.4.7
          (left OR)
          (left AND)
          (left LT GT LE GE EQ NOTEQ)
          (right CONCAT)
          (left + -)
          (left * / %)
          ; UNM: to give higher precedence to the unary negation.
          (left NOT \# UNM)
          (right ^))
   
   ; Lua's grammar
   ; TODO:
   ; 1 shift/reduce conflict: -> prefixexp . ( (yacc shifts, which is OK)
   ; 2 reduce/reduce conflicts: both, because of fcalls as stat or exp (we put
   ; rules of fcall as exp first, so yacc favours fcalls as exps over stats)
   (grammar
    
    (chunk ((block) (begin
                      ; Reset symbol table (for debugging purposes)
                      ; TODO: abstract this into a procedure
                      (reset-symbol-table)
                      (set! actual-block (new-empty-block))
                      (set! actual-block (new-block actual-block))
                      (set! actual-block
                            (new-scope actual-block
                                       (list (string->symbol "_ENV"))))
                      $1)))
    
    (block ((stats) $1)
           ((retstat) $1)
           ((stats retstat) (add-to-block $1 $2))
           (() (skip)))

    (stats ((stat) $1)
           
           ((stats stat) (add-to-block $1 $2)))


    (prefixexp ((var) $1)
               ((\( exp \)) (parent-e $2))
               ((functioncall) $1)
               )

    (functioncall
     ((prefixexp args) (fun-call $1 $2))
     ((prefixexp : NAME args) (method-call $1 (id-name $3) $4))
     )
    
    (stat
     
     ((\;) (skip))
     
     ((varlist = explist)
      (var-assign $1 $3))
     
     ((BREAK) (break))

     ; intercept funcalls as stats to add the label $statFunCall
     ((prefixexp args) (stat-fun-call $1 $2))
     ((prefixexp : NAME args) (stat-method-call $1 (id-name $3) $4))
     
     ((beginning_do block END)
      (begin
        (set! actual-block
              (close-scopes-in-block actual-block))
        (do-end $2)))
     
     ((beginning_while exp DO block END)
      (begin
        (set! actual-block
              (close-scopes-in-block actual-block))
        (while $2 $4)))

     ((beginning_repeat block UNTIL exp)
      (begin
        (set! actual-block
              (new-scope actual-block
                         (map (lambda (id) (id-name-name id))
                              (exps-el (exps (list (id-name '$dummyVar)))))))
        (local-vars
         (exps (list (id-name '$dummyVar)))
         (exps (list (nil)))
         (add-to-block
          (var-assign
           (exps (list (id-name '$dummyVar)))
           (exps (list (func-def
                        (id-name (new-label))
                        (params (exps '()))
                        (local-vars
                         (exps (list (id-name '$dummyGuardVar)))
                         (exps (list (nmbr 0)))
                         
                         (while (binop (lt)
                                       (id-name '$dummyGuardVar)
                                       (nmbr 1))
                                (add-to-block
                                 (var-assign
                                  (exps (list (id-name '$dummyGuardVar)))
                                  (exps (make-list 1 (nmbr 1))))
                                 (add-to-block $2
                                               (conditional (unop (\\not) $4)
                                                            (stat-fun-call
                                                             (id-name '$dummyVar)
                                                             (exps '()))
                                                            (skip)))))
                                  )))))
          (stat-fun-call (id-name '$dummyVar)
                         (exps '()))
          ))))
     
     ((LOCAL namelist) (begin
                         ; New scope
                         (set! actual-block
                               (new-scope actual-block
                                          (map (lambda (id) (id-name-name id))
                                               (exps-el $2))))
                          
                         (local-vars $2
                                     (exps (make-list (length (exps-el $2))
                                                      (nil)))
                                     (skip))))
     
     ((LOCAL namelist = explist) (begin
                                   (set! actual-block
                                         (new-scope actual-block
                                                    (map (lambda (id) (id-name-name id))
                                                         (exps-el $2))))
                                    
                                   (local-vars $2
                                               $4
                                               (skip))))
     
     ((if_guard_branch END) $1)
     
     ((if_guard_branch else_elseif END) (conditional (conditional-guard $1)
                                                     (conditional-if $1)
                                                     $2))

     ; global func def
     ; NOTE: the funcname non-terminal was deleted an its productions' rhs
     ; form part of the following productions' lhs (to be able to recognize
     ; the dotsepnamelist \: NAME case
     ((FUNCTION dotsepnamelist funcbody) (var-assign (exps (list $2)) (exps (list $3))))
     
     ((FUNCTION method_name funcbody)
      (begin
        ; Close the block with one scope that has the "self" symbol
        (set! actual-block (close-scopes-in-block actual-block))
        (var-assign (exps (list $2))
                    ; Add the implicit self parameter
                    (exps
                     (list
                      (func-def
                       (func-def-label $3)
                       (match (func-def-formal-params $3)
                         ((params ids)
                          (params (exps (append (list (id-name 'self))
                                                (exps-el ids)))))
                         ((vararg-params ids)
                          (vararg-params (exps (append (list (id-name 'self))
                                                       (exps-el ids))))))
                       (func-def-body $3)))))))
     
     ((local_function funcbody) (local-vars (local-vars-ids $1)
                                            (local-vars-exps $1)
                                            (var-assign (local-vars-ids $1)
                                                        (exps (list $2)))))
     
     ; From Lua 5.2's reference manual:
     ; do
     ;    local var, limit, step = tonumber(e1), tonumber(e2), tonumber(e3)
     ;    if not (var and limit and step) then error() end
     ;    while (step > 0 and var <= limit) or (step <= 0 and var >= limit) do
     ;      local v = var
     ;      block
     ;      var = var + step
     ;    end
     ; end
     
     ; TODO: abstract this into a procedure?
     ((numeric_for_beginning block END)
      (begin
        (set! actual-block (close-scopes-in-block actual-block))
                                           
        (do-end
         (local-vars
          ; lvalues
          (exps (list (id-name '$var) (id-name '$limit)
                                 (id-name '$step)))
          ; rvalues
          (exps (map (lambda (exp) (built-in-call (id-name 'tonumber)
                                                  (exps (list exp
                                                              (nil)))))
                                (list-tail $1 1)))
          ; body
          (conc-stats
           (list (conditional
                  ; guard
                  (unop (\\not)
                        (binop (\\and)
                               
                               (binop
                                (\\and)
                                (id-name '$var)
                                (id-name '$limit))
                               
                               (id-name '$step)))
                  ; if branch
                  (return (exps (list (built-in-call (id-name 'error)
                                                     (exps (list (nil)))))))
                  ; else branch
                  (skip))
                 
                 (while (binop (\\or)
                               (binop (\\and)
                                      (binop (gt)
                                             (id-name '$step)
                                             (nmbr 0.0))
                                      (binop (le)
                                             (id-name '$var)
                                             (id-name '$limit)))
                               
                               (binop (\\and)
                                      (binop (le)
                                             (id-name '$step)
                                             (nmbr 0.0))
                                      (binop (ge)
                                             (id-name '$var)
                                             (id-name '$limit))))
                        
                        (local-vars
                         ; lvalues
                         (exps (list (list-ref $1 0)))
                         ; rvalues
                         (exps (list (id-name '$var)))
                         ; body
                         (add-to-block $2
                                       ; Increment
                                       (var-assign
                                        (exps (list (id-name '$var)))
                                        (exps (list (binop (add)
                                                           (id-name '$var)
                                                           (id-name '$step))))))
                         )))
           )))))
     
     ((generic_for_beginning block END)
      (begin
        (set! actual-block (close-scopes-in-block actual-block))
                                           
        (do-end
         (local-vars
          ; lvalues
          (exps (list (id-name '$f) (id-name '$s) (id-name '$var)))
          ; rvalues
          (list-ref $1 1)
          ; body
          (while
           ; guard
           (true)
           ; body
           (local-vars
            ; lvalues
            (list-ref $1 0)
            ; rvalues
            (exps (list (fun-call (id-name '$f)
                                  (exps (list (id-name '$s)
                                              (id-name '$var))))))
            ; body
            (add-to-block
             (conc-stats
              (list
               (conditional
                (binop (equ)
                       (list-ref (exps-el (list-ref $1 0)) 0)
                       (nil))
                (break)
                (skip))
               (var-assign
                ; lvalues
                (exps (list (id-name '$var)))
                ; rvalues
                (exps (list (list-ref (exps-el (list-ref $1 0)) 0))))))
             $2)))))))
     
     )
    
    (generic_for_beginning
     ((FOR namelist IN explist DO)
      (begin
        (set! actual-block (new-block actual-block))
        (set! actual-block
              (new-scope actual-block (map (lambda (elem) (id-name-name elem))
                                           (exps-el $2))))
        (list $2 $4)
        )))
    
    (numeric_for_beginning
     ((FOR NAME = exp \, exp \, exp DO)
      (begin
        (set! actual-block (new-block actual-block))
        (set! actual-block
              (new-scope actual-block (list '$var '$limit '$step $2)))
        (list (id-name $2) $4 $6 $8)
        ))
     
     ; step not specified. Default to 1.
     ((FOR NAME = exp \, exp DO)
      (begin
        (set! actual-block (new-block actual-block))
        (set! actual-block
              (new-scope actual-block (list '$var '$limit '$step $2)))
        (list (id-name $2) $4 $6 (nmbr 1.0))
        )))

    ; Signature of a method
    (method_name
     ((dotsepnamelist \: NAME)
      (begin
        (set! actual-block (new-block actual-block))
        (set! actual-block (new-scope actual-block (list 'self)))
        (var-table-field $1 (str (symbol->string $3))))))
    
    (local_function
     ((LOCAL FUNCTION NAME) (begin
                              (set! actual-block
                                    (new-scope actual-block (list $3)))
                               
                              (local-vars (exps (list (id-name $3)))
                                          (exps (list (nil)))
                                          (void)))))
    
    (beginning_if
      ((IF) (set! actual-block (new-block actual-block))))
    
    (beginning_else
      ((ELSE) (set! actual-block (new-block actual-block))))
    
    (beginning_elseif
      ((ELSEIF) (set! actual-block (new-block actual-block))))
    
    ; TODO: simplify
    (if_guard_branch
     ((beginning_if exp THEN block)
      (begin
        (set! actual-block (close-scopes-in-block actual-block))
        (conditional $2 $4 (skip)))))
    
    (elseif_guard_branch
     ((beginning_elseif exp THEN block)
      (begin
        (set! actual-block (close-scopes-in-block actual-block))
        (conditional $2 $4 (skip)))))
    
    (else_elseif
     ((beginning_else block)
      (begin
        (set! actual-block (close-scopes-in-block actual-block))
        $2))
     
     ((elseif_guard_branch) $1)
     
     ((elseif_guard_branch else_elseif) (begin
                                          (conditional (conditional-guard $1)
                                                       (conditional-if $1)
                                                       $2))))
    
    (beginning_while 
      ((WHILE) (prec BEGINNING_WHILE)
               (set! actual-block (new-block actual-block))))
    
    (beginning_do
      ((DO) (prec BEGINNING_DO_END)
            (set! actual-block (new-block actual-block))))

    (beginning_repeat
      ((REPEAT) (prec BEGINNING_REPEAT)
            (set! actual-block (new-block actual-block))))

    
    (var
     ((NAME) (translate-var $1))
     
     ((prefixexp \[ exp \]) (var-table-field $1 $3))
     
     ((prefixexp \. NAME)
      (var-table-field $1
                       ; Just in case the lexer replaced "_" for "~", like in
                       ; the metatable-related keys
                       ;(str (string-replace (symbol->string $3) "~" "_"))))
                       (str (symbol->string $3))))
     )
    
    ;
    ;
    ;stat
    ;  
    ;  //| label                             {}
    ;  //| GOTO NAME                         {}
    ;  
    ;  
    ;
    
    ;                                        
    ;  //| REPEAT block UNTIL exp
    ;  
    ;
    ;//label
    ;//  : '::' NAME '::'
    ;
    
    
    
    (funcbody_formal_parameters
     ((\( parlist \))
      (begin
        (set! actual-block (new-block actual-block))
        (set! actual-block
              (new-scope actual-block
                         (map (lambda (id) (id-name-name id))
                              (exps-el (match $2
                                         ((params ids) ids)
                                         ((vararg-params ids) ids))))))
        $2))
     
     ; We add this production to avoid obfuscating the code from the previous
     ; one, trying to distinguish an empty parlist from a non-empty one.
     ((\( \)) (begin
                (set! actual-block (new-block actual-block))
                (params (exps '())))))
    
    (funcbody
     ((funcbody_formal_parameters block END)
      (begin
        (set! actual-block
              (close-scopes-in-block actual-block))
        (func-def (id-name (new-label)) $1 $2)
        )))
    
    (retstat ((RETURN) (return (exps (list (nil)))))
             ((RETURN \;) (return (exps (list (nil)))))
             ((RETURN explist) (return $2))
             ((RETURN explist \;) (return $2)))
    
    (explist ((exp) (exps (list $1)))
             ((explist \, exp) (exps (append (exps-el $1)
                                             (list $3)))))
    
    (exp ((VARARG) (id-vararg))
         ((STRING) (str $1))
         ((NUMBER) (nmbr $1))
         ((NIL) (nil))
         ((TRUE) (true))
         ((FALSE) (false))
         ((exp + exp) (binop (add) $1 $3))
         ((exp - exp) (binop (sub) $1 $3))
         ((exp * exp) (binop (mul) $1 $3))
         ((exp / exp) (binop (div) $1 $3))
         ((exp % exp) (binop (mod) $1 $3))
         ((exp ^ exp) (binop (pow) $1 $3))
         ((exp LT exp) (binop (lt) $1 $3))
         ((exp LE exp) (binop (le) $1 $3))
         ((exp GT exp) (binop (gt) $1 $3))
         ((exp GE exp) (binop (ge) $1 $3))
         ((exp EQ exp) (binop (equ) $1 $3))
         ((exp NOTEQ exp) (unop (\\not) (binop (equ) $1 $3)))
         ((exp CONCAT exp) (binop (str-concat) $1 $3))
         ((exp AND exp) (binop (\\and) $1 $3))
         ((exp OR exp) (binop (\\or) $1 $3))
         ((NOT exp) (unop (\\not) $2))
         ((\# exp) (unop (len) $2))
         ;  // NOTE: the precedence of a rule is determined by that of its last terminal
         ;  // symbol. In this case, the tokens that represent the operators. That's why
         ;  // we cannot have just one production for binary operators, using a 
         ;  // non-terminal binop, like in Lua's grammar
         ;  | SUB exp %prec UNM   {$$ = new e_pointer(new_unop_e(unary_operator::UNM, *$2));}
         ((- exp) (prec UNM) (unop (unm) $2))
         ((tableconstructor) $1)
         ((prefixexp) $1)
         ((functiondef) $1)
         )
    
    (varlist
     ((var) (exps (list $1)))
     
     ((varlist \, var) (exps (append (exps-el $1)
                                     (list $3))))
     )

    ; Productions added to solve shift/reduce conflicts
    (fcallbegin
     ((prefixexp) $1))

    
    (args
     ((\( explist \)) $2)
     ((\( \)) (exps '()))
     ((tableconstructor) (exps (list $1)))
     ((STRING) (exps (list (str $1))))
     )
    
    (namelist
     ((NAME) (exps (list (id-name $1))))
     
     ((namelist \, NAME) (exps (append (exps-el $1)
                                       (list (id-name $3))))))
    
    (parlist
     ((namelist) (params $1))
     ((namelist \, VARARG) (vararg-params $1))
     ((VARARG) (vararg-params (exps '()))))
    
    (func_signature
     ((FUNCTION \( parlist \))
      (begin
        ; New scope
        (set! actual-block (new-block actual-block))
        
        (set! actual-block
              (new-scope actual-block
                         (map (lambda (id) (id-name-name id))
                              (match $3
                                ((params ids) (exps-el ids))
                                
                                ((vararg-params ids) (exps-el ids))))))
        
                                 $3))
     
     ((FUNCTION \( \)) (begin
                         ; New scope
                         (set! actual-block (new-block actual-block))
                          
                         (params (exps '())))))
    
    (functiondef
     ((func_signature block END)
      (begin
        (set! actual-block (close-scopes-in-block actual-block))
        (func-def (id-name (new-label)) $1 $2))))
    
    (dotsepnamelist
     ((NAME) (translate-var $1))
     
     ((dotsepnamelist \. NAME)
      ; Just in case the lexer replaced "_" for "~".
      ;(var-table-field $1 (str (string-replace (symbol->string $3) "~" "_")))))
      (var-table-field $1 (str (symbol->string $3)))))
    
    (tableconstructor
     ((\{ fieldlist \}) (tableconstructor $2))
     ((\{ \}) (tableconstructor (fields '()))))
    
    (fieldlist ((field) (fields (list $1)))
               
               ((fieldlist fieldsep field) (fields (append (fields-flds $1)
                                                           (list $3))))
               ((fieldlist fieldsep) $1))
    
    (fieldsep ((\,) (void))
              
              ((\;) (void)))
    
    (field ((\[ exp \] = exp) (kv-table-field $2 $5))
           ((NAME = exp) (kv-table-field
                          ; Just in case the lexer replaced "_" for "~", like in the
                          ; metatable-related keys
                          ;(str (string-replace (symbol->string $1) "~" "_")) $3))
                          (str (symbol->string $1)) $3))
           ((exp) (v-table-field $1)))
    )
   
   ))

(provide lua-parser)


;                                                                          
;                             ;     ;;;       ;                            
;                                     ;                                    
;      ;                              ;                                    
;     ; ;                             ;                                    
;     ; ;   ;    ;  ;;  ;;  ;;;       ;     ;;;       ;;;    ;;;;   ;    ; 
;     ; ;   ;    ;   ;  ;     ;       ;       ;      ;   ;   ;;  ;   ;   ; 
;    ;   ;  ;    ;    ;;      ;       ;       ;          ;   ;       ;  ;  
;    ;   ;  ;    ;    ;;      ;       ;       ;      ;;;;;   ;       ;  ;  
;    ;;;;;  ;    ;    ;;      ;       ;       ;     ;    ;   ;        ; ;  
;   ;;   ;; ;   ;;   ;  ;     ;       ;       ;     ;   ;;   ;        ;;   
;   ;     ;  ;;; ;  ;;  ;;  ;;;;;      ;;;  ;;;;;    ;;; ;   ;         ;   
;                                                                      ;   
;                                                                     ;    
;                                                                    ;;    

; concatenates stat to block, taking into account scoping rules
(define (add-to-block block stat)
  (match block
    ((conc-stats stats)
     ; Check if the last stat is local var decl. In which case,
     ; the new stat belongs to its scope
     (match (last stats)
       ((local-vars ids exps scope)
        (conc-stats
         (append (take stats (- (length stats) 1))
                 (list (local-vars ids
                                   exps
                                   (add-to-block scope stat))))))
       
       (_
        ; Check stat
        (match stat
          ; block of statements
          ((conc-stats stats2)
           (conc-stats (append stats stats2)))

          ; single statement
          (_
           (conc-stats (append stats (list stat))))
        ))))
    
    ((local-vars ids exps scope)
     (local-vars ids
                 exps
                 (add-to-block scope stat)))
    
    ; single statement
    (_
     ; check stat
     (match stat
       ; block of statements
       ((conc-stats stats)
        (conc-stats (append (list block) stats)))
       
       ; single statement
       (_
        (conc-stats (list block stat)))
        )
     ))
  )

(define (translate-var var)
  (if (is-in-block actual-block var)
      ; it's a local variable
      (begin
        (if (equal? var (string->symbol "_ENV"))
          global-env-subst
          (id-name var)))
      
      ; it's a global variable
      ; Replace ~ for _.
      ;(var-table-field global-env-subst (str (string-replace (symbol->string var) "~" "_")))
      (var-table-field global-env-subst (str (symbol->string var)))
      ))

; from doc of 2 LALR(1) Parsers: The result of a parser expression with one
; start non-terminal is a function, parse, that takes one argument. This
; argument must be a zero argument function, gen, that produces successive
; tokens of the input each time it is called. 
(define (lex-this lexer input) (lambda () (lexer input)))

; parses "input" string, allowing for the replacement of the global environment
; id (if the parser is invoked at "runtime", i.e., from within a Lua program)
; example:
; > (parse-this "a = 1" #t (term (to-abstract (ref 1))))
; '(((ref 1) |[| "a" |]|) = 1.0)
; > (parse-this "a = 1" #f (void))
; '((_ENV |[| "a" |]|) = 1.0)
(define (parse-this input runtime? ref)
  (if runtime?
      (set! global-env-subst ref)
      (set! global-env-subst (id-name '_ENV)))

  (define counting-lines-port (open-input-string input))
  
  (port-count-lines! counting-lines-port)
  
  (define res
        (concrete-grammar-s
         (lua-parser (lex-this lua-lexer counting-lines-port))))

  ; cleaning symbol table
  (set! actual-block (new-empty-block))

  (set! actual-block (new-block actual-block))

  (set! actual-block
        (new-scope actual-block
                   (list (string->symbol "_ENV"))))
  
  res)

(provide parse-this)