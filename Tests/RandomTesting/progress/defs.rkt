#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../Relations/fullProgs.rkt"
         "../../../Relations/terms.rkt"
         "../../../Relations/termsValStore.rkt"
         "../../../Meta-functions/delta.rkt"
         "../../../Meta-functions/substitution.rkt"
         "../../../Meta-functions/valStoreMetafunctions.rkt"
         "../../../Meta-functions/objStoreMetafunctions.rkt"
         "./prepare.rkt"
         )

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
(define-judgment-form
  ext-lang
  #:mode (well_formed_conf_table_field I I I I)
  #:contract (well_formed_conf_table_field C σ θ (side-condition
                                                  any
                                                  (is_term? (term any))))
  
  [(well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   ---------------------------------------------------------------------------
   (well_formed_conf_table_field any σ θ ((\[ e_1 \] = e_2)))]
  
  [(well_formed_term any σ θ e)
   ---------------------------------------------------------------------------
   (well_formed_conf_table_field any σ θ (e))]

  [(well_formed_term any σ θ e_1)
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
  #:contract (well_formed_term C σ θ (side-condition
                                      any
                                      (is_term? (term any))))


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

  [(side-condition ,(or (redex-match ext-lang
                                     (in-hole C_2 (while e do C_3 end))
                                     (term any))
                        
                        (redex-match ext-lang
                                     (in-hole C_2 (C_3 Break))
                                     (term any))))
   ---------------------------------------------------------------
   (well_formed_term any σ θ break)]

  [-----------------------------------------------------------------
   (well_formed_term any σ θ (return))]

  [(well_formed_term any σ θ e) ...
   -----------------------------------------------------------------
   (well_formed_term any σ θ (return e ...))]

  ; fun call
  [(well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   ----------------------------------------------------------------------
   (well_formed_term any σ θ ($statFunCall e_1 (e_2 ...)))]

  [(well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   --------------------------------------------------------------------
   (well_formed_term any σ θ ($statFunCall e_1 : Name (e_2 ...)))]
  
  ; var assignment
  [(well_formed_term any σ θ var) ...
   (well_formed_term any σ θ e) ...
   ----------------------------------------------------------------------
   (well_formed_term any σ θ (var ... = e ...))]

  ; do-end
  [(well_formed_term any σ θ s)
   ----------------------------------------------------------------------
   (well_formed_term any σ θ (do s end))]

  [(well_formed_term any σ θ e)
   (well_formed_term any σ θ s_1)
   (well_formed_term any σ θ s_2)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (if e then s_1 else s_2 end))]

  [(well_formed_term any σ θ e)
   (well_formed_term any σ θ s)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (while e do s end))]

  ; local var
  [(well_formed_term any σ θ e)
   (well_formed_term ,(plug (term any)
                            (term (local Name ... = e in hole end))) σ θ s)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (local Name ... = e in s end))]

  [(well_formed_term any σ θ e) ...
   (well_formed_term ,(plug (term any)
                            (term (local Name ... = e ... in
                                    hole end))) σ θ s)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (local Name ... = e ... in s end))]

  [(well_formed_term any σ θ r)
   (well_formed_term ,(plug (term any)
                            (term (hole ((rEnv r)) LocalBody))) σ θ s)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (s ((rEnv r)) LocalBody))]

  [(well_formed_term any σ θ renv) ...
   (well_formed_term ,(plug (term any)
                            (term (hole (renv ...)
                                        LocalBody))) σ θ s)
   --------------------------------------------------------------------------
   (well_formed_term any σ θ
                     (s (renv ...) LocalBody))]

  ; conc stats
  [(well_formed_term any σ θ s_1)
   (well_formed_term any σ θ s_2)
   (well_formed_term any σ θ s_3)
   ...
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (s_1 s_2 s_3 ...))]

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
   ; Is s_1 a statement that represents the execution of a while loop?
   (side-condition ,(or (redex-match? ext-lang
                                      ($iter e do s_2 end)
                                      (term s_1))

                        (redex-match? ext-lang
                                      (if e_1 then
                                          (s_2
                                           ($iter e_1 do s_2 end))
                                          else \; end)
                                      (term s_1))

                        (redex-match? ext-lang
                                      (s_2 ($iter e do s_2 end))
                                      (term s_1))

                        (is_skip? (term s_1))
                        ))
   --------------------------------------------------------------------------
   (well_formed_term any σ θ (s_1 Break))]

  ; $iter
  [(well_formed_term any σ θ e)
   (well_formed_term ,(plug (term any)
                            (term ($iter e do hole end))) σ θ s)
   (side-condition ,(redex-match? ext-lang
                                  (in-hole C_2 (C_3 Break))
                                  (term any)))
   --------------------------------------------------------------------------
   (well_formed_term any σ θ ($iter e do s end))]

  ; table assignment, wrong key
  [(well_formed_term any σ θ objref) ; checks for membership of objref to θ
   (well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   
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
                                            end))) σ θ s)
   ----------------------------------------------
   (well_formed_term any σ θ (function Name_1 (Name_2 ...) s end))]

  [(well_formed_term ,(plug (term any)
                            (term (function Name_1 (Name_2 ... <<<)
                                            hole
                                            end))) σ θ s)
   ----------------------------------------------
   (well_formed_term any σ θ (function Name_1 (Name_2 ... <<<) s end))]

  ; vararg mark
  [(side-condition ,(redex-match ext-lang
                                 (in-hole C_2 (function Name_1 (Name_2 ... <<<)
                                                        C_3 end))
                                 (term any)))
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ <<<)]

  ; A Name's occurrence must be bounded
  [(side-condition
    ,(or (redex-match ext-lang
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
         (redex-match ext-lang
                      (in-hole C_2
                               (local Name_1 ...
                                 (side-condition Name_2
                                                 (equal? (term Name_2)
                                                         (term Name_4)))
                                 Name_3 ... = e ... in C_3 end))
                      (term any))))
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ Name_4)]

  [(well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ (e_1 \[ e_2 \]))]

  ; built-in service
  
  [---------------------------------------------------------------------------
   (well_formed_term any σ θ ($builtIn builtinserv ()))]
  
  [(well_formed_term any σ θ e)
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ ($builtIn builtinserv (e)))]

  [(well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2)
   (well_formed_term any σ θ e_3)
   ...
   ---------------------------------------------------------------------------
   (well_formed_term any σ θ ($builtIn builtinserv (e_1 e_2 e_3 ...)))]

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
  [(well_formed_term any σ θ e_1)
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
  
  [(well_formed_term any σ θ e) ...
   --------------------------------------------------
   (well_formed_term any σ θ (< e ... >))]
  ; fun call
  [(well_formed_term any σ θ e)
   ----------------------------------------------------------------------
   (well_formed_term any σ θ (e ()))]
  
  [(well_formed_term any σ θ e_1)
   (well_formed_term any σ θ e_2) ...
   ----------------------------------------------------------------------
   (well_formed_term any σ θ (e_1 (e_2 ...)))]

  [(well_formed_term any σ θ e_1)
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

  [(well_formed_term any σ θ e)
   ; e should be a function call or any intermediate state of a function call
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
   (well_formed_term any σ θ (e ProtectedMode))]

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
  well-formed-vsp : vsp σ θ -> any

  [(well-formed-vsp (r v) σ θ)
   #t
   
   ; value must be well formed
   (side-condition (judgment-holds (well_formed_term hole σ θ v)))
   ]

  ; default
  [(well-formed-vsp any ...)
   #f]
  )

(define-metafunction ext-lang
  well_formed_sigma : σ σ θ -> any
  
  [(well_formed_sigma () σ θ)
   #t]

  [(well_formed_sigma ((r v)) σ θ)
   #t

   (side-condition (term (well-formed-vsp (r v) σ θ)))]

  [(well_formed_sigma ((refStdout String)) σ θ)
   #t]

  ; stdout file
  [(well_formed_sigma ((refStdout String) (r_1 v_2) (r_2 v_3) ...) σ θ)
   (well_formed_sigma ((r_1 v_2) (r_2 v_3) ...) σ θ)
   ]
  
  ; Stores are functions: for their syntactic representation, we ask for their
  ; domains to contain as refs natural numbers in strictly increasing order
  [(well_formed_sigma (((ref natural_1) v_1)
                       ((ref natural_2) v_2) (r v_3) ...) σ θ)
   (well_formed_sigma (((ref natural_2) v_2) (r v_3) ...) σ θ)

   (side-condition (< (term natural_1)
                      (term natural_2)))

   (side-condition (term (well-formed-vsp ((ref natural_1) v_1) σ θ)))]

  [(well_formed_sigma any ...)
   #f])

(define-metafunction ext-lang
  well_formed_osp : osp σ θ -> any

  [(well_formed_osp (tid_1 (tableconstructor tid_2 pos)) σ θ)
   #t
   
   ; meta-table tid_2 must not be removed before tid_1
   (side-condition (term (refBelongsToTheta? tid_2 θ)))

   ; table constructor must be well formed
   (side-condition (judgment-holds (well_formed_term hole σ θ 
                                                     tableconstructor)))
   ]

  [(well_formed_osp (tid_1 (tableconstructor nil pos)) σ θ)
   #t
   
   ; table constructor must be well formed
   (side-condition (judgment-holds (well_formed_term hole σ θ 
                                                     tableconstructor)))
   ]

  [(well_formed_osp (cid functiondef) σ θ)
   #t
   
   ; functiondef must be well formed
   (side-condition (judgment-holds (well_formed_term hole σ θ 
                                                     functiondef)))
   ]

  ; default
  [(well_formed_osp any ...)
   #f]
  )

(define-metafunction ext-lang
  well_formed_theta : θ σ θ -> any
  
  [(well_formed_theta () σ θ)
   #t]

  [(well_formed_theta (osp) σ θ)
   #t

   (side-condition (term (well_formed_osp osp σ θ)))]

  ; simple check to enforce θ as functions
  [(well_formed_theta (((any_1 natural_1) any_2)
                       ((any_3 natural_2) any_4) osp ...) σ θ)
   (well_formed_theta (((any_3 natural_2) any_4) osp ...) σ θ)

   (side-condition (< (term natural_1)
                      (term natural_2)))

   (side-condition (term (well_formed_osp ((any_1 natural_1) any_2) σ θ)))]

  [(well_formed_theta any ...)
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
   (side-condition (not (or (redex-match ext-lang
                                         (in-hole E_2 ((in-hole Elf hole)
                                                       (renv ...) RetStat))
                                         (term E))
                            
                            (redex-match ext-lang
                                         (in-hole E_2 ((in-hole Elf hole)
                                                       (renv ...) RetExp))
                                         (term E))

                            (redex-match ext-lang
                                         (in-hole E_2 ((in-hole Elf hole)
                                                       Break))
                                         (term E)))))
   ]

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

  ; The concept depends only on the stat
  [(is_final_conf (σ : θ : s))
   (is_final_stat s)]

  [(is_final_conf (σ : θ : v))
   #t]
  )

(provide is_final_conf)

(define (check_conf debug conf result)
  (if debug
        (begin
          (print conf)
          (println (term (well_formed_conf ,conf)))
          )
        (or
         ; it was a final configuration 
         (and (= (length result) 0)
              (term (is_final_conf ,conf)))
         ; not a final configuration 
         (and (= (length result) 1)
              (term (well_formed_conf ,(first result)))))))

; terms-rel
(define (check_one_step_term_rel debug t result)
  (if debug
        (begin
          (print t)
          (println (term (well_formed_conf (() : () : ,t))))
          )
        (or
         ; it was a final configuration 
         (and (= (length result) 0)
              (term (is_final_conf (() : () : ,t))))
         ; not a final configuration 
         (and (= (length result) 1)
              (term (well_formed_conf ,(first result)))))))

(define (soundness_wfc_pred_term t debug)
  (let ([result (if (is_s? (term ,t))
                    ; {(is_s? (term ,t))}
                    (if
                     (not (term (well_formed_conf (() : () : ,t))))
                     ; TODO: naive approach to discard ill formed
                     ; terms
                     (term ((() : () : \;)))
                     
                     (apply-reduction-relation full-progs-rel
                                               (term (() : () : ,t))))
                    ; {¬ (is_s? (term ,t))}
                    (if
                     (not (term (well_formed_conf (() : () : ,t))))
                     ; TODO: naive approach to discard ill formed
                     ; terms
                     (term ((() : () : \;)))
                     
                     (apply-reduction-relation full-progs-rel
                                               ; generate a term that implies
                                               ; the reduction of t
                                               (term (() : () : (if ,t then \;
                                                                    else \;
                                                                    end))))))
                    ])
    (if (is_s? (term ,t))
        (check_one_step_term_rel debug t result)
        (check_one_step_term_rel debug (term (if ,t then \;
                                                 else \;
                                                 end)) result))
    )
  )

(define (soundness_wfc_terms attempts)
  (redex-check ext-lang t
               (soundness_wfc_pred_term (term t) #f)
               #:prepare close_term
               #:attempts attempts
               #:source terms-rel
               ))

; val-terms-rel
(define (check_one_step_val_store_term_rel debug σ t result)
  (if debug
        (begin
          (print σ)
          (print t)
          (println (term (well_formed_conf (,σ : () : ,t))))
          )
        (or
         ; it was a final configuration 
         (and (= (length result) 0)
              (term (is_final_conf (,σ : () : ,t))))
         ; not a final configuration 
         (and (= (length result) 1)
              (term (well_formed_conf ,(first result)))))))

(define (soundness_wfc_pred_val_store_term σ t debug)
  (let ([result (if (is_s? (term ,t))
                    ; {(is_s? (term ,t))}
                    (if
                     (not (term (well_formed_conf (,σ : () : ,t))))
                     ; TODO: naive approach to discard ill formed
                     ; terms
                     (term ((() : () : \;)))
                     
                     (apply-reduction-relation full-progs-rel
                                               (term (,σ : () : ,t))))
                    ; {¬ (is_s? (term ,t))}
                    (if
                     (not (term (well_formed_conf (,σ : () : ,t))))
                     ; TODO: naive approach to discard ill formed
                     ; terms
                     (term ((() : () : \;)))
                     
                     (apply-reduction-relation full-progs-rel
                                               ; generate a term that implies
                                               ; the reduction of t
                                               (term (,σ : () : (if ,t then \;
                                                                    else \;
                                                                    end))))))
                    ])
    (if (is_s? (term ,t))
        (check_one_step_val_store_term_rel debug σ t result)
        (check_one_step_val_store_term_rel debug σ
                                           (term (if ,t then \;
                                                     else \;
                                                     end)) result))
    )
  )

(define (soundness_wfc_val_store_terms attempts)
  (redex-check ext-lang (σ : t)
               (soundness_wfc_pred_val_store_term (term σ) (term t) #f)
               #:prepare close_term
               #:attempts attempts
               #:source terms-val-store
               ))



; full-progs-rel
(define (soundness_wfc_pred sigma theta t debug)
  (let ([result (if
                 (not (term (well_formed_conf (,sigma : ,theta : ,t))))
                 ; TODO: naive approach to discard ill formed
                 ; terms
                 (term ((() : () : \;)))
                                 
                 (apply-reduction-relation
                  full-progs-rel
                  (term (,sigma : ,theta : ,t))))])
    (check_conf debug (term (,sigma : ,theta : ,t)) result)
    )
  )



(define (soundness_wfc attempts)
  (redex-check ext-lang (σ : θ : s) ;#:uniform-at-random 0.2
               (soundness_wfc_pred (term σ) (term θ) (term s) #f)
               #:prepare close_conf
               #:attempts attempts
               #:source full-progs-rel
               ))

(define (soundness_wfc_coverage attempts test rel)
  ; create records to register test coverage related with ↦
  (let ([rel-coverage (make-coverage rel)]
        [full-progs-rel-coverage (make-coverage full-progs-rel)])
    (parameterize
        ; supply data-structures
        ([relation-coverage (list rel-coverage full-progs-rel-coverage)])
      (test attempts)
      (values (covered-cases rel-coverage)
              (covered-cases full-progs-rel-coverage)))))

;                  
;      ;;          
;     ;            
;     ;            
;     ;            
;   ;;;;;   ;    ; 
;     ;     ;;  ;; 
;     ;      ;  ;  
;     ;      ;  ;  
;     ;      ;;;;  
;     ;       ;;   
;     ;       ;;   
;                  
;                  
;                  

(define-metafunction ext-lang
  [(free_var? <<< any)
   ,(and (redex-match? ext-lang
                       (in-hole C <<<)
                       (term any))
         
         (not (redex-match? ext-lang
                            (in-hole C_1 (function Name_1 (Name_2 ... <<<)
                                                   (in-hole C_2 <<<) end))
                            (term any))))]
  
  [(free_var? Name any)
   ,(and (redex-match? ext-lang
                       (in-hole C Name)
                       (term any))
         
         (not (redex-match? ext-lang
                            (in-hole E (local var ... Name var ... = e ... in
                                         (in-hole C Name) end))
                            (term any))))]
  )

(define-metafunction ext-lang
  [(free_vars? () any)
   #t]
  
  [(free_vars? (id_1 id_2 ...) any)
   ,(or (term (free_var? id_1 any))
        (term (free_vars? (id_2 ...) any)))])

(define (fv_correctness attempts)
  (redex-check ext-lang s
               (term (free_vars? (fv s) s))
               #:attempts attempts))