#lang racket

(require redex
         "../../../grammar.rkt")

; terms as compiled from Lua code: needed to enforce well-formedness of
; programs;
(define-extended-language well-formed-lang ext-lang
  
  [scoresing \;
             break
             (return ecore ...)
             ($statFCall ecore (ecore ...))
             ($statFCall ecore : Name (ecore ...))
             (corevar_1 corevar_2 ... = ecore ...)
             (do scoreblock end)
             (if ecore then scoreblock else scoreblock end)
             (while ecore do scoreblock end)
             ($iter ecore do scoreblock end)
             (local Name_1 Name_2 ... = ecore ... in scoreblock end)]

  ; it rules out programs like ((s_1 statlabel) ((s_2 statlabel)), which could
  ; get stuck
  [scoreblock scoresing
              (scoresing_1 scoresing_2 scoresing_3 ...)]


  [ecore v
         <<<
         Name 
         (ecore \[ ecore \])
         (\( ecore \))
         ($builtIn builtinserv (ecore ...))
         tableconstructor
         (ecore binop ecore)
         (unop ecore)
         functiondef
         (ecore (ecore ...))
         (ecore : Name (ecore ...))
         ; run-time expressions (need to be added since environment is
         ; manipulated through substitution)
         r]

  [corevar Name 
           (ecore \[ ecore \])]
  )

(provide well-formed-lang)               