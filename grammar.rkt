#lang racket

(require redex
         "coreGrammar.rkt")

(define-extended-language ext-lang core-lang
  [sing ....
        ; extensions
        (s Break)
        ; to help with the definition of well-formed programs, we exclude as
        ; many ill-formed programs as possible,  using the grammar
        (($statFCall v (v ...)) Meta objid ...)
        (((v \[ v \]) = v) Meta objid ...)
        
        (((tid \[ v \]) = v) WrongKey objid ...)
        (((v \[ v \]) = v) NonTable objid ...)
        (($statFCall v (v ...)) WFunCall objid ...)
        ; renv is not an expression nor a value.
        (s (renv ...) LocalBody)
        ; to allow intermediate states of execution of a funtioncall
        (s (renv ...) RetStat)
        
        ; error objects
        ($err v)]

  [v ....
     tid
     cid]

  [e ....
     ; run-time expressions
     r
     (< e ... >)
     ($err v)
     ; renv is not an expression nor a value. The previous rules for these
     ; constructions does not describe the renv added
     (s (renv ...) RetExp)
     (e ProtMD v)
     (e ProtMD) ; protected mode where the handler has been used
     
     ; to help with the definition of well-formed programs, we exclude with the
     ; grammar as many ill-formed programs as possible
     ((v (v ...)) Meta objid ...)
     ((not (v (v ...))) Meta objid ...)
     ((not (not (v (v ...)))) Meta objid ...)
     ((\( (v (v ...)) \)) Meta objid ...)
     ((v \[ v \]) Meta objid ...)

     ((v_1 (v_2 ...)) WFunCall objid ...)
     ((v \[ v \]) NonTable objid ...)
     ((tid \[ v \]) WrongKey objid ...)
     ((v arithop v) BinopWO objid ...)
     ((v .. v) BinopWO objid ...)
     ((v < v) BinopWO objid ...)
     ((v <= v) BinopWO objid ...)
     ((- v) NegWrongOp objid ...)
     ((\# v) StrLenWrongOp objid ...)
     ((v == v) EqFail objid ...)]

  ; identifiers of variables and refs., to ease the definition of several
  ; substitution functions
  [id ....
      r
      tid
      cid]

  [label statlabel
         explabel]
  
  [statlabel WrongKey ;metamethods
             NonTable
             WFunCall]

  [explabel WrongKey
            NonTable
            BinopWO
            EqFail
            NegWrongOp
            StrLenWrongOp
            WFunCall]

  ; tables
  [(efield ef) (\[ v \] = v)
               v]
  
  [evaluatedtable (\{ efield ... \})]

  [evar r
        (v \[ v \])]

  [var ....
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
       (if Elf then s else s end)
       (local Name_1 Name_2 ... = v ... Elf e ... in s end)
       (Elf (renv ...) LocalBody)
       (evar ... (Elf \[ e \]) var ... = e ...)
       (evar ... (v \[ Elf \]) var ... = e ...)
       (evar_1 evar_2 ... = v ... Elf e ...)
       (return v ... Elf e ...)
       (Elf sing_1 sing_2 ...)   

       ; Function call, method call, built-in services
       ($statFCall Elf (e ...))
       ($statFCall v (v ... Elf e ...))
       ($statFCall Elf : Name (e ...))
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
     (in-hole Elf (E ProtMD v))
     (in-hole Elf (E ProtMD))
     ]

  ; tuples
  ; list of expressions where a tuple is truncated
  [Etel (v ... hole e_1 e_2 ...)]
  
  ; immediate evaluation contexts where a tuple is truncated
  [Et (if hole then s else s end)
      (local Name_1 Name_2 ... = v ... hole e_1 e_2 ... in s end)
      (evar ... (hole \[ e \]) var ... = e ...)
      (evar ... (v \[ hole \]) var ... = e ...)
      (evar_1 evar_2 ... = v ... hole e_1 e_2 ...)
      (return v ... hole e_1 e_2 ...)
      (hole (e ...))
      (v Etel)
      ($statFCall hole (e ...))
      ($statFCall v Etel)
      ($statFCall hole : Name (e ...))
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
  [Eael  (v ... hole)]
  ; immediate evaluation contexts where a tuple is unwrapped
  [Ea (local Name_1 Name_2 ... = v ... hole in s end)
      (return v ... hole)
      (evar_1 evar_2 ... = v ... hole)
      (v (v ... hole))
      ($statFCall v Eael)
      ($builtIn builtinserv Eael)
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

  ; locations
  [l r
     tid
     cid]
  
  [C hole
     ; Statements
     (do C end)
     (if C then s else s end)
     (if e then C else s end)
     (if e then s else C end)
     (local Name ... = e ... C e ... in s end)
     (local Name ... = e ... in C end)
     (e ... C e ... = e ...)
     (e ... = e ... C e ...)
     (return e ... C e ...)
     (while C do s end)
     (while e do C end)
     ; Function call, method call, built-in services
     ($statFCall C (e ...))
     ($statFCall e (e ... C e ...))
     ($statFCall C : Name (e ...))
     ($statFCall e : Name (e ... C e ...))

     (C sing_1 sing_2 ...)
     (sing_1 sing_2 ... C sing_3 ...)
     
     ; Run-time
     (C (renv ...) RetStat)
     (s (renv_1 ... (rEnv C) renv_2 ...) RetStat)
     (C (renv ...) RetExp)
     (s (renv_1 ... (rEnv C) renv_2 ...) RetExp)
     (C (renv ...) LocalBody)
     (s (renv_1 ... (rEnv C) renv_2 ...) LocalBody)
     (C Break)
     (C ProtMD v)
     (e ProtMD C)
     ($err C)
     ($iter C do s end)
     ($iter e do C end)

     ; added each production, instead of a single (C statlabel) and
     ; (C explabel), to avoid redundancy errors
     (($statFCall C (v ...)) Meta objid ...)
     (($statFCall v (v ... C v ...)) Meta objid ...)
     (((C \[ v \]) = v) Meta objid ...)
     (((v \[ C \]) = v) Meta objid ...)
     (((v \[ v \]) = C) Meta objid ...)
         
     (((C \[ v \]) = v) WrongKey objid ...)
     (((tid \[ C \]) = v) WrongKey objid ...)
     (((tid \[ v \]) = C) WrongKey objid ...)
     (((C \[ v \]) = v) NonTable objid ...)
     (((v \[ C \]) = v) NonTable objid ...)
     (((v \[ v \]) = C) NonTable objid ...)
     (($statFCall C (v ...)) WFunCall objid ...)
     (($statFCall v (v ... C v ...)) WFunCall objid ...)
     ((C (v ...)) WFunCall objid ...)
     ((v (v ... C v ...)) WFunCall objid ...)

     ; (C explabel)
     ((C (v ...)) Meta objid ...)
     ((v (v ... C v ...)) Meta objid ...)
     ((not (C (v ...))) Meta objid ...)
     ((not (v (v ... C v ...))) Meta objid ...)
     ((not (not (C (v ...)))) Meta objid ...)
     ((not (not (v (v ... C v ...)))) Meta objid ...)
     ((\( (C (v ...)) \)) Meta objid ...)
     ((\( (v (v ... C v ...)) \)) Meta objid ...)
     ((C \[ v \]) Meta objid ...)
     ((v \[ C \]) Meta objid ...)
     
     ((C \[ v \]) NonTable objid ...)
     ((v \[ C \]) NonTable objid ...)
     ((C \[ v \]) WrongKey objid ...)
     ((tid \[ C \]) WrongKey objid ...)
     ((C arithop v) BinopWO objid ...)
     ((v arithop C) BinopWO objid ...)
     ((C .. v) StrConcatWrongOps objid ...)
     ((v .. C) StrConcatWrongOps objid ...)
     ((C < v) OrdCompWrongOps objid ...)
     ((v < C) OrdCompWrongOps objid ...)
     ((C <= v) OrdCompWrongOps objid ...)
     ((v <= C) OrdCompWrongOps objid ...)
     ((- C) NegWrongOp objid ...)
     ((\# C) StrLenWrongOp objid ...)
     ((C == v) EqFail objid ...)
     ((v == C) EqFail objid ...)
     
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