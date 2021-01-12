#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Relations/fullProgs.rkt"
         "../../../Relations/terms.rkt"
         "../../../Relations/termsValStore.rkt"
         "../../../Relations/termsObjStore.rkt"
         "../../../Relations/termsValObjStore.rkt"
         "../../../Relations/meta.rkt"
         "../../../Meta-functions/delta.rkt"
         "../../../Meta-functions/substitution.rkt"
         "../../../Meta-functions/objStoreMetafunctions.rkt"
         "./prepare.rkt")

;                                                                                          
;                                                                                          
;                   ;;;     ;;;                ;;                                        ; 
;                     ;       ;               ;                                          ; 
;                     ;       ;               ;                                          ; 
;                     ;       ;               ;                                          ; 
;  ;      ;  ;;;;     ;       ;             ;;;;;    ;;;;    ;;;;   ;;;;;;;  ;;;;    ;;;;; 
;  ;      ; ;;  ;;    ;       ;               ;     ;;  ;;   ;;  ;  ;  ;  ; ;;  ;;  ;;  ;; 
;   ; ;; ;  ;    ;    ;       ;               ;     ;    ;   ;      ;  ;  ; ;    ;  ;    ; 
;   ; ;; ;  ;;;;;;    ;       ;      ;;;      ;     ;    ;   ;      ;  ;  ; ;;;;;;  ;    ; 
;   ; ;; ;  ;         ;       ;               ;     ;    ;   ;      ;  ;  ; ;       ;    ; 
;    ;  ;   ;;   ;    ;       ;               ;     ;;  ;;   ;      ;  ;  ; ;;   ;  ;;  ;; 
;    ;  ;    ;;;;      ;;;     ;;;            ;      ;;;;    ;      ;  ;  ;  ;;;;    ;;;;; 
;                                                                                          
;                                                                                          
;                                                                                          
;
; stored tables have more constraints than regular tableconstructor; the
; following formal systems checks them
(define-judgment-form
  ext-lang
  #:mode (well_formed_stored_table I I I I)
  #:contract (well_formed_stored_table C σ θ (field ...))

  ; rule added to simplify the use of this system
  [---------------------------------------------------------------------------
   (well_formed_stored_table any σ θ ())]

  [; key must not be nil or nan, value must not be nil
   (side-condition ,(not (or (is_nil? (term e_1))
                             (equal? (term e_1)
                                     +nan.0)
                             (is_nil? (term e_2)))))
   
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   (well_formed_stored_table any σ θ (field ...))
   ---------------------------------------------------------------------------
   (well_formed_stored_table any σ θ ((\[ e_1 \] = e_2) field ...))]
  )

(define-judgment-form
  ext-lang
  #:mode (well_formed_conf_table_field I I I I)
  #:contract (well_formed_conf_table_field C σ θ (field ...))

  [; valid intermediate state of computation
   (side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   ---------------------------------------------------------------------------
   (well_formed_conf_table_field any σ θ ((\[ e_1 \] = e_2)))]
  
  [(well_formed_term any σ θ e)
   ---------------------------------------------------------------------------
   (well_formed_conf_table_field any σ θ (e))]

  [; valid intermediate state of computation
   (side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   (well_formed_conf_table_field any σ θ (field_1 field_2 ...))
   ---------------------------------------------------------------------------
   (well_formed_conf_table_field any σ θ ((\[ e_1 \] = e_2) field_1
                                                            field_2 ...))]

  [(well_formed_term any σ θ e)
   (well_formed_conf_table_field any σ θ (field_1 field_2 ...))
   ---------------------------------------------------------------------------
   (well_formed_conf_table_field any σ θ (e field_1 field_2 ...))]
  )



(define-judgment-form
  ext-lang
  #:mode (well_formed_term I I I I)
  #:contract (well_formed_term C σ θ t)


  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;             ;               ;            
  ;             ;               ;            
  ;    ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
  ;   ;    ;    ;      ;   ;    ;     ;    ; 
  ;   ;         ;          ;    ;     ;      
  ;    ;;;;     ;      ;;;;;    ;      ;;;;  
  ;        ;    ;     ;    ;    ;          ; 
  ;   ;    ;    ;     ;   ;;    ;     ;    ; 
  ;    ;;;;      ;;;   ;;; ;     ;;;   ;;;;  
  ;                                          
  ;                                          
  ;                                          
  ;                                          


  [-------------------------------
   (well_formed_term any σ θ \;)]

  [(side-condition
    ,(or (redex-match? ext-lang
                       (side-condition
                        (in-hole C_2 (while e do C_3 end))
                        (not (or (redex-match? ext-lang
                                               (in-hole C_4 (C_5 (renv ...) RetStat))
                                               (term  C_3))
                                 (redex-match? ext-lang
                                               (in-hole C_4 (C_5 (renv ...) RetExp))
                                               (term  C_3))
                                 (redex-match? ext-lang
                                               (in-hole C_4 (function Name parameters C_5 end))
                                               (term  C_3)))))
                       (term any))
         ; considers an aprox. def. of Elf in ((in-hole Elf break) Break) and
         ; breaks outside fun defs
         (redex-match? ext-lang
                       (side-condition
                        (in-hole C_2 (C_3 Break))
                        (not (or (redex-match? ext-lang
                                               (in-hole C_4 (C_5 (renv ...) RetStat))
                                               (term  C_3))
                                 (redex-match? ext-lang
                                               (in-hole C_4 (C_5 (renv ...) RetExp))
                                               (term  C_3))
                                 (redex-match? ext-lang
                                               (in-hole C_4 (function Name parameters C_5 end))
                                               (term  C_3)))))
                       (term any))))
   ---------------------------------------------------------------
   (well_formed_term any σ θ break)]

  [-----------------------------------------------------------------
   (well_formed_term any σ θ (return))]

  [; e_1 e_2 ... must be a legitimate intermediate state of computation
   (side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   -----------------------------------------------------------------
   (well_formed_term any σ θ (return e_1 e_2 ...))]

  ; fun call
  [; e_1 (e_2 ...) must be a legitimate intermediate state of evaluation
   (side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   ----------------------------------------------------------------------
   (well_formed_term any σ θ ($statFunCall e_1 (e_2 ...)))]

  [; e_1 (e_2 ...) must be a legitimate intermediate state of evaluation
   (side-condition ,(redex-match? ext-lang
                                  ; after the evaluation of e_1, the method call
                                  ; is rewritten into a function call over some
                                  ; table value; the list of parameters is
                                  ; evaluated only after this transformation
                                  (ecore ...)
                                  (term (e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   --------------------------------------------------------------------
   (well_formed_term any σ θ ($statFunCall e_1 : Name (e_2 ...)))]
  
  ; var assignment
  [; var ... = e ... must be a legitimate intermediate state of evaluation
   (side-condition ,(redex-match? ext-lang
                                  ; it should match with at least one of the
                                  ; following patterns
                                  ((evar_1 ... var_2 ... ecore_1 ...) ...
                                   (evar_2 ... v ... e_2 ecore_2 ...) ...)
                                  (term ((var_1 ... e_1 ...)))))
   ; the following checks if a var of the form e \[ e \] is also a valid
   ; intermediate state of computation
   (well_formed_term any σ θ var_1) ...
   (well_formed_term any σ θ e_1) ...
   ----------------------------------------------------------------------
   (well_formed_term any σ θ (var_1 ... = e_1 ...))]

  ; do-end
  [(well_formed_term any σ θ s)
   ----------------------------------------------------------------------
   (well_formed_term any σ θ (do s end))]

  [(well_formed_term any σ θ e)
   (well_formed_term any σ θ scoreblock_1)
   (well_formed_term any σ θ scoreblock_2)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (if e then scoreblock_1 else scoreblock_2 end))]

  [(well_formed_term any σ θ e)
   (well_formed_term ,(plug (term any)
                            (term (while e do hole end))) σ θ scoreblock)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (while e do scoreblock end))]

  ; local var
  [(well_formed_term ,(plug (term any)
                            (term (local Name ... = in hole end))) σ θ s)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (local Name ... = in s end))]
  
  [; checks if e ... is a valid intermediate state of evaluation
   (side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   (well_formed_term ,(plug (term any)
                            (term (local Name ... = e_1 e_2 ... in
                                    hole end))) σ θ scoreblock)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (local Name ... = e_1 e_2 ... in scoreblock end))]

  [(well_formed_term any σ θ r) ...
   (well_formed_term ,(plug (term any)
                            (term (hole ((rEnv r) ...)
                                        LocalBody))) σ θ s)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ
                     (s ((rEnv r) ...) LocalBody))]

  ; conc stats
  [(well_formed_term any σ θ ssing)
   (well_formed_term any σ θ scoresing_1)
   (well_formed_term any σ θ scoresing_2) ...
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (ssing scoresing_1 scoresing_2 ...))]

  ; error object
  [(well_formed_term any σ θ v)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ ($err v))]

  
  ;                                                                          
  ;                                                                          
  ;   ;;;             ;                                                      
  ;     ;             ;                                                      
  ;     ;             ;                         ;               ;            
  ;     ;             ;                         ;               ;            
  ;     ;       ;;;   ;;;;;            ;;;;   ;;;;;;    ;;;   ;;;;;;   ;;;;  
  ;     ;      ;   ;  ;;  ;;          ;    ;    ;      ;   ;    ;     ;    ; 
  ;     ;          ;  ;    ;          ;         ;          ;    ;     ;      
  ;     ;      ;;;;;  ;    ;           ;;;;     ;      ;;;;;    ;      ;;;;  
  ;     ;     ;    ;  ;    ;               ;    ;     ;    ;    ;          ; 
  ;     ;     ;   ;;  ;;  ;;          ;    ;    ;     ;   ;;    ;     ;    ; 
  ;      ;;;   ;;; ;  ;;;;;            ;;;;      ;;;   ;;; ;     ;;;   ;;;;  
  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;                                                                          
  

  ; Break tag
  [(well_formed_term ,(plug (term any)
                            (term (hole Break))) σ θ 
                                                 s_1)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (s_1 Break))]

  ; $iter
  [(well_formed_term any σ θ e)
   (well_formed_term ,(plug (term any)
                            (term ($iter e do hole end))) σ θ scoreblock)
   ;   (side-condition ,(redex-match? ext-lang
   ;                                  (in-hole C_2 (C_3 Break))
   ;                                  (term any)))
   --------------------------------------------------------------------------
   (well_formed_term any σ θ ($iter e do scoreblock end))]

  ; table assignment, wrong key
  [(well_formed_term any σ θ objref) ; checks for membership of objref to θ
   (well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)

   ; v_1 should not belong to dom(θ (objref))
   (side-condition ,(is_nil? (term (δ rawget objref v_1 θ))))
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (((objref \[ v_1 \]) = v_2) WrongKey))]

  ; table assignment, nontable
  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   (well_formed_term any σ θ v_3)
   (side-condition ,(not (is_tid? (term v_1))))
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (((v_1 \[ v_2 \]) = v_3) NonTable))]

  ; WrongFunCall
  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2) ...
   (side-condition ,(not (is_cid? (term v_1))))
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (($statFunCall v_1 (v_2 ...)) WrongFunCall))]
  
  [(well_formed_term ,(plug (term any)
                            (term (hole ((rEnv r) ...) RetStat))) σ θ s)
   (well_formed_term any σ θ r) ...
   ------------------------------------------------------------------------
   (well_formed_term any σ θ (s ((rEnv r) ...) RetStat))]

  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;;  ;    ; 
  ;   ;    ;    ;;    ;    ;  ;      
  ;   ;;;;;;    ;;    ;    ;   ;;;;  
  ;   ;         ;;    ;    ;       ; 
  ;   ;;   ;   ;  ;   ;;  ;;  ;    ; 
  ;    ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;                   ;              
  ;                   ;              
  ;                   ;              
  ;                            

  [--------------------------------
   (well_formed_term any σ θ nil)]

  [--------------------------------
   (well_formed_term any σ θ Boolean)]

  [--------------------------------
   (well_formed_term any σ θ Number)]

  [--------------------------------
   (well_formed_term any σ θ String)]

  [(side-condition (refBelongsToTheta? objref θ))
   ----------------------------------------------
   (well_formed_term any σ θ objref)]

  [(side-condition (refBelongsToTheta? cid θ))
   ----------------------------------------------
   (well_formed_term any σ θ cid)]

  ; functiondef
  [(well_formed_term ,(plug (term any)
                            (term (function Name_1 (Name_2 ...)
                                            hole
                                            end))) σ θ scoreblock)
   ----------------------------------------------
   (well_formed_term any σ θ (function Name_1 (Name_2 ...) scoreblock end))]

  [(well_formed_term ,(plug (term any)
                            (term (function Name_1 (Name_2 ... <<<)
                                            hole
                                            end))) σ θ scoreblock)
   ----------------------------------------------
   (well_formed_term any σ θ (function Name_1 (Name_2 ... <<<) scoreblock end))]

  ; vararg mark
  [; <<< is being captured, according to the scoping rules codified in fv
   (side-condition ,(not (member (term <<<)
                                 (term (fv ,(plug (term any) (term <<<)))))))
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ <<<)]

  ; A Name's occurrence must be bounded
  [(side-condition
    ,(or (redex-match? ext-lang
                       (in-hole C_2
                                (function Name_1
                                          (Name_2 ...
                                           (side-condition
                                            Name_3
                                            (equal? (term Name_3)
                                                    (term Name_4)))
                                           any_2 ...)
                                          C_3 end))
                       (term any))
         (redex-match? ext-lang
                       (in-hole C_2
                                (local Name_1 ...
                                  (side-condition Name_2
                                                  (equal? (term Name_2)
                                                          (term Name_4)))
                                  Name_3 ... = e ... in C_3 end))
                       (term any))))
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ Name_4)]

  [; (e_1 \[ e_2 \]) must represent a valid intermediate state of evaluation
   (side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ (e_1 \[ e_2 \]))]

  ; built-in service
    [---------------------------------------------------------------------------
     (well_formed_term any σ θ ($builtIn builtinserv ()))]

  [; e_1 ... must represent a valid intermediate state of evaluation
   (side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ ($builtIn builtinserv (e_1 e_2 ...)))]

  ; parenthesized expression
  [(well_formed_term any σ θ e)
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ (\( e \)))]

  ; table constructor and fields
  [---------------------------------------------------------------------------
   (well_formed_term any σ θ (\{ \}))]
  
  [(well_formed_conf_table_field any σ θ (field_1 field_2 ...))
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ (\{ field_1 field_2 ... \}))]


  ; binop
  [(side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ (e_1 binop e_2))]

  ; unop
  [(well_formed_term any σ θ e)
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ (unop e))]

  ; val ref
  [-----------------------------------
   (well_formed_term any ((any_1 v_1) ... (r v_2) (any_2 v_3) ...) θ r)]

  [(well_formed_term any σ θ r)
   -----------------------------------
   (well_formed_term any σ θ (rEnv r))]

  ; tuples
  [--------------------------------------------------
   (well_formed_term any σ θ (< >))]
  
  [(side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   --------------------------------------------------
   (well_formed_term any σ θ (< e_1 e_2 ... >))]
  ; fun call
  [(side-condition ,(redex-match? ext-lang
                                  (v ... e_3 ecore ...)
                                  (term (e_1 e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   ----------------------------------------------------------------------
   (well_formed_term any σ θ (e_1 (e_2 ...)))]

  [(side-condition ,(redex-match? ext-lang
                                  ; after the evaluation of e_1, the method call
                                  ; is rewritten into a function call over some
                                  ; table value; the list of parameters is
                                  ; evaluated only after this transformation
                                  (ecore ...)
                                  (term (e_2 ...))))
   (well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   --------------------------------------------------------------------
   (well_formed_term any σ θ (e_1 : Name (e_2 ...)))]
  
  ;                                                                  
  ;                                                                  
  ;   ;;;             ;                                              
  ;     ;             ;                                              
  ;     ;             ;                                              
  ;     ;             ;                                              
  ;     ;       ;;;   ;;;;;            ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;     ;      ;   ;  ;;  ;;          ;;  ;;   ;  ;   ;;  ;;  ;    ; 
  ;     ;          ;  ;    ;          ;    ;    ;;    ;    ;  ;      
  ;     ;      ;;;;;  ;    ;          ;;;;;;    ;;    ;    ;   ;;;;  
  ;     ;     ;    ;  ;    ;          ;         ;;    ;    ;       ; 
  ;     ;     ;   ;;  ;;  ;;          ;;   ;   ;  ;   ;;  ;;  ;    ; 
  ;      ;;;   ;;; ;  ;;;;;            ;;;;   ;;  ;;  ;;;;;    ;;;;  
  ;                                                   ;              
  ;                                                   ;              
  ;                                                   ;              
  ;                                                                  

;  [(well_formed_term any σ θ e)
;   ; e should be a function call or any intermediate state of a function call
;   (side-condition ,(or (redex-match? ext-lang
;                                      (s (renv ...) RetExp)
;                                      (term e))
;                        
;                        (redex-match? ext-lang
;                                      (< v ... >)
;                                      (term e))
;
;                        (redex-match? ext-lang
;                                      (v_1 (v_2 ...))
;                                      (term e))
;
;                        (redex-match? ext-lang
;                                      ($err v)
;                                      (term e))
;
;                        (redex-match? ext-lang
;                                      ((v_1 (v_2 ...)) WrongFunCall)
;                                      (term e))))
;   ------------------------------------------------------------
;   (well_formed_term any σ θ (e ProtectedMode))]

  [(well_formed_term any σ θ e)
   (well_formed_term any σ θ v)
   (side-condition ,(or (redex-match? ext-lang
                                      (s (renv ...) RetExp)
                                      (term e))
                        
                        (redex-match? ext-lang
                                      (< v ... >)
                                      (term e))

                        (redex-match? ext-lang
                                      (v_1 (v_2 ...))
                                      (term e))

                        (redex-match? ext-lang
                                      ($err v)
                                      (term e))

                        (redex-match? ext-lang
                                      ((v_1 (v_2 ...)) WrongFunCall)
                                      (term e))))
   ------------------------------------------------------------
   (well_formed_term any σ θ (e ProtectedMode v))]

  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   (side-condition ,(not (is_tid? (term v_1))))
   ------------------------------------------------------
   (well_formed_term any σ θ ((v_1 \[ v_2 \])NonTable))]

  [(well_formed_term any σ θ objref) ; checks for (refBelongsToTheta? objref θ)
   (well_formed_term any σ θ v)
   (side-condition ,(is_nil? (term (δ rawget objref v θ))))
   ------------------------------------------------------
   (well_formed_term any σ θ ((objref \[ v \])WrongKey))]

  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   (side-condition
    ,(or (not (is_number? (term v_1)))
         (not (is_number? (term v_2)))
         (not (is_number? (term (δ tonumber v_1 nil))))
         (not (is_number? (term (δ tonumber v_2 nil))))))
   ----------------------------------------------------------------------------
   (well_formed_term any σ θ ((v_1 arithop v_2) ArithWrongOps))]

  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   (side-condition
    ,(or (not (or (is_number? (term v_1))
                  (is_string? (term v_1))))
         (not (or (is_number? (term v_2))
                  (is_string? (term v_2))))))
   ----------------------------------------------------------------------------
   (well_formed_term any σ θ ((v_1 .. v_2) StrConcatWrongOps))]

  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   (side-condition ,(not (and (equal? (term (δ type v_1))
                                      (term (δ type v_2)))
                              (or (is_string? (term v_1))
                                  (is_number? (term v_1))))))
   ---------------------------------------------------------------
   (well_formed_term any σ θ ((v_1 relop v_2) OrdCompWrongOps))]

  [(side-condition ,(not (is_string? (term v))))
   (well_formed_term any σ θ v)
   ---------------------------------------------------
   (well_formed_term any σ θ ((\# v)StrLenWrongOp))]

  [(side-condition ,(and (not (is_number? (term v)))
                         (not (is_number? (term (δ tonumber v nil))))))
   (well_formed_term any σ θ v)
   ------------------------------------------------------------------------
   (well_formed_term any σ θ ((- v)NegWrongOp))]

  [(side-condition ,(equal? (term (δ == v_1 v_2))
                            (term false)))
   (well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   ------------------------------------------------------------------------
   (well_formed_term any σ θ ((v_1 == v_2) EqFail))]

  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2) ...
   (side-condition ,(not (is_cid? (term v_1))))
   --------------------------------------------------------------------------
   (well_formed_term any σ θ ((v_1 (v_2 ...)) WrongFunCall))]

  [(well_formed_term ,(plug (term any)
                            (term (hole ((rEnv r) ...) RetExp))) σ θ s)
   (well_formed_term any σ θ r) ...
   ------------------------------------------------------------------------
   (well_formed_term any σ θ (s ((rEnv r) ...) RetExp))]
  )

(provide well_formed_term)

(define-metafunction ext-lang
  well_formed_vsp : vsp σ θ -> any

  [(well_formed_vsp (r v) σ θ)
   #t
   
   ; value must be well formed: tid and cid must belong to dom(θ)
   (side-condition (judgment-holds (well_formed_term hole σ θ v)))]

  ; default
  [(well_formed_vsp any ...)
   #f]
  )

(define-metafunction ext-lang
  well_formed_sigma : σ σ θ -> any
  
  [(well_formed_sigma () σ θ)
   #t]

  ; stdout file
  [(well_formed_sigma ((refStdout String) (r v) ...) σ θ)
   (well_formed_sigma ((r v) ...) σ θ)]
  
  [(well_formed_sigma ((r v) vsp ...) σ θ)
   (well_formed_sigma (vsp ...) σ θ)

   (side-condition (term (well_formed_vsp (r v) σ θ)))]

  ; default
  [(well_formed_sigma _ _ _)
   #f])

(define-metafunction ext-lang
  well_formed_osp : osp σ θ -> any
  
  [(well_formed_osp (tid_1 ((\{ field ... \}) tid_2 pos)) σ θ)
   #t

   ; meta-table tid_2 must not be removed before tid_1
   (side-condition (judgment-holds (well_formed_term hole σ θ tid_2)))

   ; table constructor must be well formed
   (side-condition (judgment-holds (well_formed_stored_table hole σ θ 
                                                             (field ...))))]

  [(well_formed_osp (tid_1 ((\{ field ... \}) nil pos)) σ θ)
   #t
   
   ; table constructor must be well formed
   (side-condition (judgment-holds (well_formed_stored_table hole σ θ 
                                                             (field ...))))]

  [(well_formed_osp (cid functiondef) σ θ)
   #t
   
   ; functiondef must be well formed
   (side-condition (judgment-holds (well_formed_term hole σ θ 
                                                     functiondef)))]

  ; default
  [(well_formed_osp any ...)
   #f]
  )

(define-metafunction ext-lang
  well_formed_theta : θ σ θ -> any
  
  [(well_formed_theta () σ θ)
   #t]

  [(well_formed_theta (osp_1 osp_2 ...) σ θ)
   (well_formed_theta (osp_2 ...) σ θ)

   (side-condition (term (well_formed_osp osp_1 σ θ)))]

  [(well_formed_theta _ _ _)
   #f])

(define-metafunction ext-lang
  [(well_formed_conf (σ : θ : t))
   ,(and
     (term (well_formed_sigma σ σ θ))

     (term (well_formed_theta θ σ θ))
     
     (judgment-holds
      (well_formed_term hole σ θ t)))])

;                                                                  
;                                                                  
;                                                                  
;                                                                  
;                                                                  
;                                                                  
;   ;;;;;    ;;;;    ;;;;    ;;;;;   ;;;;    ;;;;    ;;;;    ;;;;  
;   ;;  ;;   ;;  ;  ;;  ;;  ;;  ;;   ;;  ;  ;;  ;;  ;    ;  ;    ; 
;   ;    ;   ;      ;    ;  ;    ;   ;      ;    ;  ;       ;      
;   ;    ;   ;      ;    ;  ;    ;   ;      ;;;;;;   ;;;;    ;;;;  
;   ;    ;   ;      ;    ;  ;    ;   ;      ;            ;       ; 
;   ;;  ;;   ;      ;;  ;;  ;;  ;;   ;      ;;   ;  ;    ;  ;    ; 
;   ;;;;;    ;       ;;;;    ;;; ;   ;       ;;;;    ;;;;    ;;;;  
;   ;                            ;                                 
;   ;                        ;   ;                                 
;   ;                         ;;;                                  
;                                                                  
; PRE : {t is well-formed, with respect to some stores}
(define-metafunction ext-lang
  is_final_stat : s -> any
  
  [(is_final_stat (in-hole E (return v ...)))
   #t

   ; (return v ...) occurs outside of a funcall
   (side-condition (not (or (redex-match? ext-lang
                                          (in-hole E_2 ((in-hole Elf hole)
                                                        (renv ...) RetStat))
                                          (term E))
                            
                            (redex-match? ext-lang
                                          (in-hole E_2 ((in-hole Elf hole)
                                                        (renv ...) RetExp))
                                          (term E))

                            (redex-match? ext-lang
                                          (in-hole E_2 ((in-hole Elf hole)
                                                        Break))
                                          (term E)))))]

  [(is_final_stat ($err v))
   #t]

  [(is_final_stat \;)
   #t]

  ; default
  [(is_final_stat s)
   #f]
  )

(define-metafunction ext-lang
  is_final_conf : (σ : θ : t) -> any

  ; the concept depends only on the stat
  [(is_final_conf (σ : θ : s))
   (is_final_stat s)]

  [(is_final_conf (σ : θ : v))
   #t]
  )

(provide is_final_conf)

(define (check_one_step c result)
  (or
   ; it was a final configuration 
   (and (= (length result) 0)
        (term (is_final_conf ,c)))
   ; not a final configuration 
   (and (= (length result) 1)
        (term (well_formed_conf ,(first result))))))

(define (soundness_wfc_pred c debug)
  (let ([result (if (not (term (well_formed_conf ,c)))
                    ; TODO: naive approach to discard ill formed
                    ; terms
                    (if debug
                        (begin (println (term ,c))
                               (term ((() : () : \;))))
                        
                        (term ((() : () : \;))))
                     
                    (apply-reduction-relation full-progs-rel
                                              (term ,c)))])
    (check_one_step c result)
    )
  )

; generates "attempts" examples taking into account left hand-side of the rules
; from "rel"; flag "debug" indicates if the non-well-formed terms must be printed 
(define (soundness_wfc rel attempts debug)
  (redex-check ext-lang any
               (soundness_wfc_pred (term any) debug)
               #:prepare close_conf
               #:attempts attempts
               #:source rel))

; test and print relation coverage of soundness_wfc
(define (soundness_wfc_coverage rel attempts debug)
  ; create records to register test coverage related with ↦
  (let ([rel-coverage (make-coverage rel)]
        [full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list rel-coverage full-progs-rel-coverage)])
      (soundness_wfc rel attempts debug)
      (values (covered-cases rel-coverage)
              (covered-cases full-progs-rel-coverage))
      )))

; generates "attempts" examples following the pattern (σ : θ : s)
; flag "debug" indicates if the non-well-formed terms must be printed
(define (soundness_wfc_no_rel attempts debug)
  (redex-check ext-lang (σ : θ : s)
               (soundness_wfc_pred (term (σ : θ : s)) debug)
               #:prepare close_conf
               #:attempts attempts))

; tests and prints relation coverage of soundness_wfc_no_rel
(define (soundness_wfc_no_rel_coverage attempts debug)
  ; create records to register test coverage related with ↦
  (let ([full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list full-progs-rel-coverage)])
      (soundness_wfc_no_rel attempts debug)
      (values (covered-cases full-progs-rel-coverage))
      )))

; generates "attempts" examples following the pattern
; (σ : θ : (return ($builtIn builtinserv (v ...))))
; flag "debug" indicates if the non-well-formed terms must be printed
(define (soundness_wfc_builtIn attempts debug)
  (redex-check ext-lang (σ : θ : (return ($builtIn builtinserv (v ...))))
               (soundness_wfc_pred (term (σ : θ : (return ($builtIn builtinserv (v ...))))) debug)
               #:prepare close_conf
               #:attempts attempts
               ))

; test and print relation coverage soundness_wfc_builtIn
(define (soundness_wfc_builtIn_coverage attempts debug)
  ; create records to register test coverage related with ↦
  (let ([full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list full-progs-rel-coverage)])
      (soundness_wfc_builtIn attempts debug)
      (values (covered-cases full-progs-rel-coverage)))))


; TODO: any way to automate this?
(define terms-rel-rules 50)
(define terms-val-store-rules 3)
(define terms-obj-store-rules 11)
(define terms-val-obj-store-rules 3)
(define meta-rules 23)
(define full-rules (+ terms-rel-rules
                      terms-val-store-rules
                      terms-obj-store-rules
                      terms-val-obj-store-rules
                      meta-rules))
; ratio of the examples generated following the left side of rules, with respect
; to the total amount of examples
(define ratio-prepare (/ 2 3))

; divides tests among every relation, according to ratio-prepare
; tests soundness_wfc for every relation, except for full-progs-rel
; invokes soundness_wfc_no_rel for full-progs-rel
(define (soundness_wfc_full_coverage attempts debug)
  ; create records to register test coverage related with ↦
  (let ([terms-rel-coverage (make-coverage terms-rel)]
        [terms-val-store-coverage (make-coverage terms-val-store)]
        [terms-obj-store-coverage (make-coverage terms-obj-store)]
        [terms-val-obj-store-coverage (make-coverage terms-val-obj-store)]
        [meta-coverage (make-coverage meta)]
        [full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list terms-rel-coverage
                                  terms-val-store-coverage
                                  terms-obj-store-coverage
                                  terms-val-obj-store-coverage
                                  meta-coverage
                                  full-progs-rel-coverage)])
      (begin
        ; 50 rules in terms-rel
        (soundness_wfc terms-rel (floor (* (* attempts (/ terms-rel-rules
                                                          full-rules))
                                           ratio-prepare)) debug)
        ; 3 rules in terms-val-store
        (soundness_wfc terms-val-store (floor (* (* attempts (/ terms-val-store-rules
                                                                full-rules))
                                                 ratio-prepare)) debug)
        ; 11 rules in terms-obj-store
        (soundness_wfc terms-obj-store (floor (* (* attempts (/ terms-obj-store-rules
                                                                full-rules))
                                                 ratio-prepare)) debug)
        ; 3 rules in terms-val-obj-store
        (soundness_wfc terms-val-obj-store (floor (* (* attempts (/ terms-val-obj-store-rules
                                                                    full-rules))
                                                     ratio-prepare)) debug)
        ; 23 rules in meta
        (soundness_wfc meta (floor (* (* attempts (/ meta-rules
                                                     full-rules))
                                      ratio-prepare)) debug)

        ; remaining tests taken just from arbitrary terms of the grammar
        (soundness_wfc_no_rel (floor (* attempts
                                        (- 1 ratio-prepare))) debug)
        
        (values (covered-cases terms-rel-coverage)
                (covered-cases terms-val-store-coverage)
                (covered-cases terms-obj-store-coverage)
                (covered-cases terms-val-obj-store-coverage)
                (covered-cases meta-coverage)
                (covered-cases full-progs-rel-coverage))))))