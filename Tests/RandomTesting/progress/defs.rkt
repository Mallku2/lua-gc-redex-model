#lang racket

(require redex
         "../../../grammar.rkt"
         "../../../executionEnvironment.rkt"
         "../../../Relations/fullProgs.rkt"
         "../../../Relations/gc.rkt"
         "../../../Meta-functions/delta.rkt"
         "../../../Meta-functions/substitution.rkt"
         "../../../Meta-functions/gc.rkt"
         "../../../Meta-functions/valStoreMetafunctions.rkt"
         "../../../Meta-functions/objStoreMetafunctions.rkt")

(define-metafunction ext-lang
  [(close (σ : θ : s))
   (σ : θ : (local Name_1 Name_2 ... = nil in s end))

   (where (Name_1 Name_2 ...) ,(remove-duplicates (term (fv s))))]

  [(close (σ : θ : s))
   (σ : θ : (local any_1 ... any_2 ... = nil in (function dummy (<<<) s end)
              end))

   (where (any_1 ... <<< any_2 ...) ,(remove-duplicates (term (fv s))))]

  [(close any)
   any])

(define (close_term t)
  (term (close (unquote t))))

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
   
   (side-condition ,(is_nil? (term (δ (rawget objref v_1 θ)))))
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

  ; functiondef
  [(well_formed_term ,(plug (term any)
                            (term (function Name_1 (Name_2 ...)
                                            hole
                                            end))) σ θ s)
   ----------------------------------------------
   (well_formed_term any σ θ (function Name_1 (Name_2 ...) s end))]

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
  [(side-condition (refBelongsTo r σ))
   -----------------------------------
   (well_formed_term any σ θ r)]

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
   (side-condition ,(is_nil? (term (δ (rawget objref v θ)))))
   ------------------------------------------------------
   (well_formed_term any σ θ ((objref \[ v \])WrongKey))]

  [(well_formed_term any σ θ v_1)
   (well_formed_term any σ θ v_2)
   (side-condition
    ,(or (not (is_number? (term v_1)))
         (not (is_number? (term v_2)))
         (not (is_number? (term (δ (tonumber v_1 nil)))))
         (not (is_number? (term (δ (tonumber v_2 nil)))))))
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
   (side-condition ,(not (and (equal? (term (δ (type v_1)))
                                      (term (δ (type v_2))))
                              (or (is_string? (term v_1))
                                  (is_number? (term v_1))))))
   ---------------------------------------------------------------
   (well_formed_term any σ θ ((v_1 relop v_2) OrdCompWrongOps))]

  [(side-condition ,(not (is_string? (term v))))
   (well_formed_term any σ θ v)
   ---------------------------------------------------
   (well_formed_term any σ θ ((\# v)StrLenWrongOp))]

  [(side-condition ,(and (not (is_number? (term v)))
                         (not (is_number? (term (δ (tonumber v nil)))))))
   (well_formed_term any σ θ v)
   ------------------------------------------------------------------------
   (well_formed_term any σ θ ((- v)NegWrongOp))]

  [(side-condition ,(equal? (term (δ (== v_1 v_2)))
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
  well-formed-sigma : σ σ θ -> any
  
  [(well-formed-sigma () σ θ)
   #t]

  [(well-formed-sigma ((r v)) σ θ)
   #t

   (side-condition (term (well-formed-vsp (r v) σ θ)))]

  [(well-formed-sigma ((refStdout String)) σ θ)
   #t]

  ; stdout file
  [(well-formed-sigma ((refStdout String) (r_1 v_2) (r_2 v_3) ...) σ θ)
   (well-formed-sigma ((r_1 v_2) (r_2 v_3) ...) σ θ)

   ; only one stdout file
   (side-condition (not (redex-match ext-lang
                        ((r_3 v_4) ... (refStdout v_5) (r_4 v_6) ...)
                        (term ((r_1 v_2) (r_2 v_3) ...)))))
   ]
  
  ; Stores are functions: for their syntactic representation, we ask for their
  ; domains to contain as refs natural numbers in strictly increasing order
  [(well-formed-sigma (((ref natural_1) v_1)
                       ((ref natural_2) v_2) (r v_3) ...) σ θ)
   (well-formed-sigma (((ref natural_2) v_2) (r v_3) ...) σ θ)

   (side-condition (< (term natural_1)
                      (term natural_2)))

   (side-condition (term (well-formed-vsp ((ref natural_1) v_1) σ θ)))]

  [(well-formed-sigma any ...)
   #f])

(define-metafunction ext-lang
  well-formed-osp : osp σ θ -> any

  [(well-formed-osp (tid_1 (tableconstructor tid_2 pos)) σ θ)
   #t
   
   ; meta-table tid_2 must not be removed before tid_1
   (side-condition (term (refBelongsToTheta? tid_2 θ)))

   ; table constructor must be well formed
   (side-condition (judgment-holds (well_formed_term hole σ θ 
                                                     tableconstructor)))
   ]

  [(well-formed-osp (tid_1 (tableconstructor nil pos)) σ θ)
   #t
   
   ; table constructor must be well formed
   (side-condition (judgment-holds (well_formed_term hole σ θ 
                                                     tableconstructor)))
   ]

  ; default
  [(well-formed-osp any ...)
   #f]
  )

(define-metafunction ext-lang
  well-formed-theta : θ σ θ -> any
  
  [(well-formed-theta () σ θ)
   #t]

  [(well-formed-theta (osp) σ θ)
   #t

   (side-condition (term (well-formed-osp osp σ θ)))]

  [(well-formed-theta (((objr natural_1) object_1)
                       ((objr natural_2) object_2) osp ...) σ θ)
   (well-formed-theta (((objr natural_2) object_2) osp ...) σ θ)

   (side-condition (< (term natural_1)
                      (term natural_2)))

   (side-condition (term (well-formed-osp ((objr natural_1) object_1) σ θ)))]

  [(well-formed-theta any ...)
   #f])

(define-metafunction ext-lang
  [(well_formed_conf (σ : θ : s))
   ,(and
     (term (well-formed-sigma σ σ θ))

     (term (well-formed-theta θ σ θ))
     
     (judgment-holds
      (well_formed_term hole σ θ s)))])

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
; PRE : {s is well-formed, with respect to some stores}
(define-metafunction ext-lang
  is-final-stat : s -> any
  
  [(is-final-stat (in-hole E (return v ...)))
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

  [(is-final-stat ($err v))
   #t]

  [(is-final-stat \;)
   #t]

  ; default
  [(is-final-stat s)
   #f]
  )

(define-metafunction ext-lang
  is-final-conf : (σ : θ : s) -> any

  ; The concept depends only on the stat
  [(is-final-conf (σ : θ : s))
   (is-final-stat s)]
  )

(provide is-final-conf)

(define (soundness_wfc_pred sigma theta s)
  (let ([result (if
                 (not (term (well_formed_conf (,sigma : ,theta : ,s))))
                 ; TODO: naive approach to discard ill formed
                 ; terms
                 (term ((() : () : \;)))
                                 
                 (apply-reduction-relation
                  full-progs-rel
                  (term (,sigma : ,theta : ,s))))])
    (or
     ; it was a final configuration 
     (and (= (length result) 0)
          (term (if-final-conf (sigma : theta : s))))
                
     (and (= (length result) 1)
          (term (well_formed_conf ,(first result)))))))

(define (soundness_wfc attempts)
  (redex-check ext-lang (σ : θ : s)
               (soundness_wfc_pred (term σ) (term θ) (term s))
               #:prepare close_term
               #:attempts attempts
               #:source full-progs-rel
               ))

(define (soundness_wfc_coverage attempts)
  (let ([rel-coverage (make-coverage full-progs-rel)])
    (parameterize ([relation-coverage (list rel-coverage)])
      (soundness_wfc attempts)
      (values (covered-cases rel-coverage)))))

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