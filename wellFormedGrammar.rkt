#lang racket

(require redex
         "./extGrammar.rkt")

; defines the subset of well-formed statements and expressions, for
; random testing purposes
(define-extended-language well-formed-lang run-time-ext-lang

  ; well-formed single instruction core statements
  [wfscoresing \;
               break
               (return wfecore ...)
               ($statFCall wfecore (wfecore ...))
               ($statFCall wfecore : Name (wfecore ...))
               (corevar_1 corevar_2 ... = wfecore ...)
               (do wfscore end)
               (if wfecore then wfscore else wfscore end)
               (while wfecore do wfscore end)
               ($iter wfecore do wfscore end)
               (local Name_1 Name_2 ... = wfecore ... in wfscore end)]

  ; it rules out programs like ((s_1 statlabel) ((s_2 statlabel)), which could
  ; get stuck
  [wfscore wfscoresing
           (wfscoresing_1 wfscoresing_2 wfscoresing_3 ...)]

  ; well-formed single instruction statements with run-time extensions
  [wfssing wfscoresing
           
           (do wfs end)
           (return v ... wfe wfecore ...)

           ($statFCall wfe (wfecore ...))
           ($statFCall v (v ... wfe wfecore ...))
           
           ($statFCall wfe : Name (wfecore ...))
           ($statFCall v : Name (v ... wfe wfecore ...))

           (evar ... wfe corevar ... = wfecore ...)
           (evar ... = v ... wfe wfecore ...)

           (if wfe then wfscore else wfscore end)
           (while wfecore do wfscore end)
           ($iter wfecore do wfscore end)
           (local Name_1 Name_2 ... = v ... wfe wfecore ... in wfscore end)
           
           ; extensions
           (wfs Break)
           ; to help with the definition of well-formed programs, we exclude as
           ; many ill-formed programs as possible,  using the grammar
           (($statFCall v (v ...)) Meta tid ...)
           (((v \[ v \]) = v) Meta tid ...)
           
           (((tid \[ v \]) = v) WrongKey tid ...)
           (((v \[ v \]) = v) NonTable tid ...)
           (($statFCall v (v ...)) WFunCall tid ...)
           ; renv is not an expression nor a value.
           (wfs (renv ...) LocalBody)
           ; to allow intermediate states of execution of a funtioncall
           (wfs (renv ...) RetStat)
           
           ; error objects
           ($err v)]
  
  [wfs wfssing
       (wfssing wfscoresing_1 wfscoresing_2 ...)]
  


  [wfecore nil Boolean Number String tid cid
           <<<
           Name 
           (wfecore \[ wfecore \])
           (\( wfecore \))
           ($builtIn builtinserv (wfecore ...))
           (\{ wffield ... \})
           (wfecore binop wfecore)
           (unop wfecore)
           (function Name parameters wfscore end)
           (wfecore (wfecore ...))
           (wfecore : Name (wfecore ...))
           ; run-time expressions (need to be added since environment is
           ; manipulated through substitution)
           r
           (< wfecore ... >)

           ($err v)]

  [wfe wfecore

       (wfe \[ wfecore \])
       (v \[ wfe \])
       
       (\( wfe \))
       
       ($builtIn builtinserv (v ... wfe wfecore ...))

       (\{ efield ... wfe wffield ... \})
       
       (wfe binop wfecore)
       (v binop wfecore)
       
       (unop wfe)
       
       (wfe (wfecore ...))
       (v (v ... wfe wfecore ...))
       
       (wfe : Name (wfecore ...))
       (v : Name (v ... wfe wfecore ...))
       
       ; run-time expressions
       (< v ... >)
       (< v ... wfe wfecore ... >)
       ($err v)
       ; renv is not an expression nor a value. The previous rules for these
       ; constructions does not describe the renv added
       (wfs (renv ...) RetExp)
       (wfe ProtMD v)
       (wfe ProtMD) ; protected mode where the handler has been used
     
       ; to enforce well-formedness of labelled programs
       ((v (v ...)) Meta tid ...)
       ((not (v (v ...))) Meta tid ...)
       ((not (not (v (v ...)))) Meta tid ...)
       ((\( (v (v ...)) \)) Meta tid ...)
       ((v \[ v \]) Meta tid ...)

       ((v_1 (v_2 ...)) WFunCall tid ...)
       ((v \[ v \]) NonTable tid ...)
       ((tid \[ v \]) WrongKey tid ...)
       ((v arithop v) BinopWO tid ...)
       ((v .. v) BinopWO tid ...)
       ((v < v) BinopWO tid ...)
       ((v <= v) BinopWO tid ...)
       ((- v) NegWrongOp tid ...)
       ((\# v) StrLenWrongOp tid ...)
       ((v == v) EqFail tid ...)]

  [corevar Name 
           (wfecore \[ wfecore \])
           r]

  ; tables
  [wffield (\[ wfecore \] = wfecore)
         ; we need to allow fields like this
         wfecore]
    
  )

(provide well-formed-lang)               