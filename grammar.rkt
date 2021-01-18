#lang racket

(require redex)

(define-language ext-lang
  ; terms as compiled from Lua code: needed to enforce well-formedness of
  ; programs
  ; it rules out programs like ((s_1 statlabel) ((s_2 statlabel)), which could
  ; get stuck
  [scoresing \;
             break
             (return e ...)
             ($statFunCall e (e ...))
             ($statFunCall e : Name (e ...))
             (var_1 var_2 ... = e ...)
             (do scoreblock end)
             (if e then scoreblock else scoreblock end)
             (while e do scoreblock end)
             ($iter e do scoreblock end)
             (local Name_1 Name_2 ... = e ... in scoreblock end)]

  [scoreblock scoresing
              (scoresing_1 scoresing_2 scoresing_3 ...)]

  [ssing scoresing
         ; extensions
         (do s end)
         (s Break)
         ; to help with the definition of well-formed programs, we exclude as
         ; many ill-formed programs as possible,  using the grammar
         (((tid \[ v \]) = v) objid ... WrongKey objid ...)
         (((v \[ v \]) = v) objid ... NonTable objid ...)
         (($statFunCall v (v ...)) objid ... WFunCall objid ...)
         ; renv is not an expression nor a value.
         (s (renv ...) LocalBody)
         ; to allow intermediate states of execution of a funtioncall
         (s (renv ...) RetStat)

         ; error objects
         ($err v)]

  ; Lua's block of code: it helps to avoid an ambiguity in the grammar, between
  ; funcalls and concat. of stats
  [s ssing
     (ssing scoresing_1 scoresing_2 ...)]

  [v nil Boolean Number String
     tid
     cid]

  [Boolean true false]
    
  ; Number represents real (double-precision floating-point) numbers
  [Number real]
  
  [String string]

  ; difference between statements and expressions is present also at a semantics
  ; level: eg., tuples' semantics is defined taking into account if they appear
  ; as an expression or as a statement, and the same with funcall
  [ecore v
         <<<
         var
         (\( e \))
         ($builtIn builtinserv (e ...))
         tableconstructor
         (e binop e)
         (unop e)
         functiondef
         (e (e ...))
         (e : Name (e ...))
         ; run-time expressions (need to be added since environment is
         ; manipulated through substitution)
         r]
  
  [e ecore
     ; run-time expressions
     (< e ... >)
     ($err v)
     ; renv is not an expression nor a value. The previous rules for these
     ; constructions does not describe the renv added
     (s (renv ...) RetExp)
     ;(e ProtectedMode)
     (e ProtectedMode v)
     
     ; to help with the definition of well-formed programs, we exclude with the
     ; grammar as many ill-formed programs as possible
     (e objid ... explabel objid ...)
;     ((v_1 (v_2 ...)) objid ... WFunCall objid ...)
;     ((v \[ v \]) objid ... NonTable objid ...)
;     ((tid \[ v \]) objid ... WrongKey objid ...)
;     ((v arithop v) objid ... ArithWrongOps objid ...)
;     ((v .. v) objid ... StrConcatWrongOps objid ...)
;     ((v < v) objid ... OrdCompWrongOps objid ...)
;     ((v <= v) objid ... OrdCompWrongOps objid ...)
;     ((- v) objid ... NegWrongOp objid ...)
;     ((\# v) objid ... StrLenWrongOp objid ...)
;     ((v == v) objid ... EqFail objid ...)
     ]

  ; identifiers of variables and refs., to ease the definition of several
  ; substitution functions
  [id Name
      <<<
      r
      tid
      cid]

  [parameters (Name ...)
              (Name ... <<<)]

  ; This syntactic category is added to ease meta-functions' definitions. 
  [functiondef (function Name parameters scoreblock end)]

  ; Built-in services' names, for use by the random generator of terms
  [builtinserv assert
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
               xpcall
               ; math
               math.abs
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
               math.tanh
               ; package
               require
               ; string
               string.dump
               string.len
               string.rep
               string.reverse
               string.sub
               ; table
               table.concat
               table.insert
               table.pack
               table.unpack]

  [label statlabel
         explabel]
  
  [statlabel WrongKey ;metamethods
             NonTable
             WFunCall]

  [explabel WrongKey
            NonTable
            ArithWrongOps
            StrConcatWrongOps
            EqFail
            OrdCompWrongOps
            NegWrongOp
            StrLenWrongOp
            WFunCall]

  ; tables
  [field (\[ e \] = e)
         ; we need to allow fields like this
         e]

  [tableconstructor (\{ field ... \})]
  
  
  [(efield ef) (\[ v \] = v)
               v]
  
  [evaluatedtable (\{ efield ... \})]


  ; primitive operators
  [arithop + - * / ^ %]
  
  [relop < <= > >=]

  ; Not short-circuit binop
  [strictbinop arithop relop == ..]
  
  [binop strictbinop and or]
  
  [unop - not \#]
  
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]
  
  [evar r
        (v \[ v \])]

  [var Name 
       (e \[ e \])
       ; run-time expression
       evar]
  
  ; terms: s ∪ e; for better spec. of some relations
  [t s e]
    
  ;                                                  
  ;                                                  
  ;                                                  
  ;    ;;;;;    ;                                    
  ;   ;;    ;   ;                                    
  ;   ;       ;;;;;;   ;;;;    ;;;;    ;;;;    ;;;;  
  ;   ;;        ;     ;;  ;;   ;;  ;  ;;  ;;  ;    ; 
  ;    ;;;;;    ;     ;    ;   ;      ;    ;  ;      
  ;        ;;   ;     ;    ;   ;      ;;;;;;   ;;;;  
  ;         ;   ;     ;    ;   ;      ;            ; 
  ;   ;    ;;   ;     ;;  ;;   ;      ;;   ;  ;    ; 
  ;    ;;;;;     ;;;   ;;;;    ;       ;;;;    ;;;;  
  ;                                                  
  ;                                                  
  ;                                                  

  ; GC
  ; finalization
  [pos natural
       ⊥
       ⊘]

  ; we need to force tables with fields that have both, (different) keys and
  ; values
  [intreptable ((\{ (\[ v_!_ \] = v) ... \}) tid pos)
               ((\{ (\[ v_!_ \] = v) ... \}) nil pos)]

  ; References for the representation of the environment 
  [renv (rEnv r)]

  
  ; Ordinary refs
  [(r vr) (ref natural)]

  ; values store pair
  [vsp (r v)
       (refStdout String)]

  ; to force just one stdout file on σ, helping redex-check
  ; every ref must be different
  [σ ((r_!_ v) ...)
     ((refStdout String) (r_!_ v) ...)]

  ; table id
  [(objref tid) (objr natural)]

  ; closure id
  [cid (cl natural)]

  ; Elements from dom(θ)
  [objid tid
         cid]

  ; Elements from img(θ)
  [object intreptable
          functiondef]
  
  ; id-object binding: objects store's pairs  
  [osp ((objr natural) intreptable)
       ((cl natural) functiondef)]

  ; useful to force a functional rep. of stores, and help redex-check to
  ; generate well-formed stores
  [ospcont ((objr hole) intreptable)
           ((cl hole) functiondef)]
  
  [θ ((in-hole ospcont natural_!_) ...)]

  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;   ;;;;;;;                           ;;;;                    ;            
  ;   ;                                ;    ;                   ;            
  ;   ;       ;    ;                  ;        ;;;;   ; ;;;   ;;;;;;  ;;  ;; 
  ;   ;       ;;  ;;                  ;       ;;  ;;  ;;   ;    ;      ;  ;  
  ;   ;;;;;;;  ;  ;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;        ;  ;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;        ;;;;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;         ;;      ;;             ;    ; ;;  ;;  ;    ;    ;      ;  ;  
  ;   ;;;;;;;   ;;      ;;              ;;;;   ;;;;   ;    ;     ;;;  ;;  ;; 
  ;                                                                          
  ;                                                                          
  ;                                                                          

  ; Blocks without Break, RetStat, etc
  [Elf hole
       ; Statements
       (do Elf end)
       (if Elf then scoreblock else scoreblock end)
       (local Name_1 Name_2 ... = v ... Elf e ... in scoreblock end)
       (Elf (renv ...) LocalBody)
       (evar ... (Elf \[ e \]) var ... = e ...)
       (evar ... (v \[ Elf \]) var ... = e ...)
       (evar_1 evar_2 ... = v ... Elf e ...)
       (return v ... Elf e ...)
       (Elf ssing_1 ssing_2 ...)   

       ; Function call, method call, built-in services
       ($statFunCall Elf (e ...))
       ($statFunCall v (v ... Elf e ...))
       ($statFunCall Elf : Name (e ...))
       ($builtIn builtinserv (v ... Elf e ...))
       (Elf (e ...))
       (v (v ... Elf e ...))
       (Elf : Name (e ...))

       ; Expressions
       (\( Elf \))
       (Elf binop e)
       (v strictbinop Elf)
       (unop Elf)
       (< v ... Elf e ... >)
       (\{ efield ... (\[ Elf \] = e) field ... \})
       (\{ efield ... (\[ v \] = Elf) field ... \})
       (\{ efield ... Elf field ... \})
       (Elf \[ e \])
       (v \[ Elf \])]

  
  ; labelled-block, no protected mode
  [Enp Elf
       ; to avoid repeating every production of Elf
       (in-hole Elf (Enp Break))
       (in-hole Elf (Enp (renv ...) RetStat))
       (in-hole Elf (Enp (renv ...) RetExp))]


  ; all possible evaluation contexts
  [E Elf
     (in-hole Elf (E Break))
     (in-hole Elf (E (renv ...) RetStat))
     (in-hole Elf (E (renv ...) RetExp))
     ;(in-hole Elf (E ProtectedMode))
     (in-hole Elf (E ProtectedMode v))
     ]

  ; tuples
  ; list of expressions where a tuple is truncated
  [Etel (v ... hole e_1 e_2 ...)]
  
  ; immediate evaluation contexts where a tuple is truncated
  [Et (if hole then scoreblock else scoreblock end)
      (local Name_1 Name_2 ... = v ... hole e_1 e_2 ... in scoreblock end)
      (evar ... (hole \[ e \]) var ... = e ...)
      (evar ... (v \[ hole \]) var ... = e ...)
      (evar_1 evar_2 ... = v ... hole e_1 e_2 ...)
      (return v ... hole e_1 e_2 ...)
      (hole (e ...))
      (v Etel)
      ($statFunCall hole (e ...))
      ($statFunCall v Etel)
      ($statFunCall hole : Name (e ...))
      ($builtIn builtinserv Etel)
      (hole : Name (e ...))
      (hole binop e)
      (v strictbinop hole)
      (unop hole)
      (< v ... hole e_1 e_2 ... >)
      (\{ efield ... hole field_1 field_2 ... \})
      (\{ efield ... (\[ hole \] = e) field ... \})
      (\{ efield ... (\[ v \] = hole) field ... \})
      (hole \[ e \])
      (v \[ hole \])
      ; this contexts introduces shift/reduce and reduce/reduce conflicts
      ; (\( hole \))
      ]
  
  ; list of expressions where a tuple is unwrapped
  [Euel (v ... hole)]
  ; immediate evaluation contexts where a tuple is unwrapped
  [Eu (local Name_1 Name_2 ... = v ... hole in scoreblock end)
      (return v ... hole)
      (evar_1 evar_2 ... = v ... hole)
      (v (v ... hole))
      ($statFunCall v (v ... hole))
      ($builtIn builtinserv Euel)
      (< v ... hole >)
      (\{ efield ... hole \})]
  ;                  
  ;                  
  ;                  
  ;                  
  ;                  
  ;    ;;;;;    ;;;  
  ;   ;;  ;;   ;   ; 
  ;   ;    ;  ;      
  ;   ;    ;  ;      
  ;   ;    ;  ;      
  ;   ;;  ;;   ;   ; 
  ;    ;;; ;    ;;;  
  ;        ;         
  ;    ;   ;         
  ;     ;;;
  
  [cte tid
       cid]
  
  [C hole
     ; Statements
     (do C end)
     (if C then scoreblock else scoreblock end)
     (if e then C else scoreblock end)
     (if e then scoreblock else C end)
     (local Name ... = e ... C e ... in scoreblock end)
     (local Name ... = e ... in C end)
     (e ... C e ... = e ...)
     (e ... = e ... C e ...)
     (return e ... C e ...)
     (while C do scoreblock end)
     (while e do C end)
     ; Function call, method call, built-in services
     ($statFunCall C (e ...))
     ($statFunCall e (e ... C e ...))
     ($statFunCall C : Name (e ...))
     ($statFunCall e : Name (e ... C e ...))

     (C ssing_1 ssing_2 ...)
     (ssing_1 ssing_2 ... C ssing_3 ...)
     
     ; Run-time
     (C (renv ...) RetStat)
     (s (renv_1 ... (rEnv C) renv_2 ...) RetStat)
     (C (renv ...) RetExp)
     (s (renv_1 ... (rEnv C) renv_2 ...) RetExp)
     (C (renv ...) LocalBody)
     (s (renv_1 ... (rEnv C) renv_2 ...) LocalBody)
     (C Break)
     ;(C ProtectedMode)
     (C ProtectedMode v)
     (e ProtectedMode C)
     ($err C)
     ($iter C do scoreblock end)
     ($iter e do C end)

     ; Added each production, instead of a single (C statlabel) and
     ; (C explabel), to avoid redundancy errors
     (((C \[ v \]) = v) WrongKey)
     (((tid \[ C \]) = v) WrongKey)
     (((tid \[ v \]) = C) WrongKey)
     (((C \[ v \]) = v) NonTable)
     (((v \[ C \]) = v) NonTable)
     (((v \[ v \]) = C) NonTable)
     (($statFunCall C (v ...)) WFunCall)
     (($statFunCall v (v ... C v ...)) WFunCall)
     ((C (v ...)) WFunCall)
     ((v (v ... C v ...)) WFunCall)

     ; (C explabel)
     ((C \[ v \]) NonTable)
     ((v \[ C \]) NonTable)
     ((C \[ v \]) WrongKey)
     ((tid \[ C \]) WrongKey)
     ((C arithop v) ArithWrongOps)
     ((v arithop C) ArithWrongOps)
     ((C .. v) StrConcatWrongOps)
     ((v .. C) StrConcatWrongOps)
     ((C < v) OrdCompWrongOps)
     ((v < C) OrdCompWrongOps)
     ((C <= v) OrdCompWrongOps)
     ((v <= C) OrdCompWrongOps)
     ((- C) NegWrongOp)
     ((\# C) StrLenWrongOp)
     ((C == v) EqFail)
     ((v == C) EqFail)
     
     (< e ... C e ... >)
     
     ; Function call, method call, built-in services
     (C (e ...))
     (e (e ... C e ...))
     ($builtIn builtinserv (e ... C e ...))
     (C : Name (e ...))
     (e : Name (e ... C e ...))
  
     ; Expressions
     (\( C \))
     (C binop e)
     (e binop C)
     (unop C)
     (\{ field ... (\[ C \] = e) field ... \})
     (\{ field ... (\[ e \] = C) field ... \})
     (\{ field ... C field ... \})
     (C \[ e \])
     (e \[ C \])
     (function Name_1 (Name_2 ...) C end)
     (function Name_1 (Name_2 ... <<<) C end)
     ]  
  )
(provide ext-lang)


;                             
;                             
;                             
;      ;                      
;     ; ;                     
;     ; ;                     
;     ; ;    ;     ;  ;;   ;; 
;     ; ;    ;     ;   ;   ;  
;    ;   ;   ;     ;    ; ;   
;    ;   ;   ;     ;     ;    
;    ;;;;;   ;     ;     ;    
;    ;   ;   ;     ;    ; ;   
;   ;     ;  ;;   ;;   ;   ;  
;   ;     ;   ;;;; ;  ;;   ;; 
;                             
;                             
;                             
;                             
                               
(define is_s?
  (redex-match? ext-lang
                s))

(define is_e?
  (redex-match? ext-lang
                e))

(define (is_term? t)
  (or (is_s? t)

      (is_e? t)))


; values
(define is_v?
  (redex-match? ext-lang
                v))

(define is_number?
  (redex-match? ext-lang
                Number))

(define is_string?
  (redex-match? ext-lang
                String))

(define is_nil?
  (redex-match? ext-lang
                nil))

(define is_false?
  (redex-match? ext-lang
                false))

; values that are interpreted as false, in a boolean context
(define (is_false_cond? t)
  (or (is_false? t)
      (is_nil? t)))

(define is_true?
  (redex-match? ext-lang
                true))

(define (is_tid? t)
  (redex-match? ext-lang
                tid
                t))

(define is_cid?
  (redex-match? ext-lang
                cid))

(define is_fdef?
  (redex-match? ext-lang
                functiondef))


; operators
(define is_strconcat?
  (redex-match? ext-lang
                ..))

(define is_arithop?
  (redex-match? ext-lang
                arithop))

; exps
(define is_r?
  (redex-match? ext-lang
                r))

; statements
(define is_skip?
  (redex-match? ext-lang
                \;))

(define is_break?
  (redex-match? ext-lang
                break))

; state
(define is_intreptable?
  (redex-match? ext-lang
                intreptable))

(define (is_cte? t)
  (or (is_tid? t)

      (is_cid? t)))

(define is_theta?
  (redex-match? ext-lang
                θ))

(define is_conf?
  (redex-match? ext-lang
                (σ : θ : s)))


(provide is_s? is_e? is_term?
         ;values
         is_v?
         is_number?
         is_string?
         is_tid?
         is_cid?
         is_fdef?
         is_nil?
         is_false?
         is_true?
         is_false_cond?

         ; ops
         is_strconcat?
         is_arithop?

         ; stats
         is_skip?
         is_break?
         
         is_r?
         is_intreptable? 
         is_cte?
         is_theta?
         is_conf? )