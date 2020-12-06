#lang racket
; Expressions that interact with the values' store

(require redex
         "../grammar.rkt"
         "../Meta-functions/substitution.rkt"
         "../Meta-functions/valStoreMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         )


(define terms-val-obj-store
  (reduction-relation
   ext-lang
   #:domain (side-condition (σ : θ : any) (is_term? (term any)))
   
   ;                                                          
   ;      ;;                                   ;;;     ;;;    
   ;     ;                                       ;       ;    
   ;     ;                                       ;       ;    
   ;     ;                                       ;       ;    
   ;   ;;;;;   ;    ;  ; ;;;     ;;;     ;;;     ;       ;    
   ;     ;     ;    ;  ;;   ;   ;   ;   ;   ;    ;       ;    
   ;     ;     ;    ;  ;    ;  ;            ;    ;       ;    
   ;     ;     ;    ;  ;    ;  ;        ;;;;;    ;       ;    
   ;     ;     ;    ;  ;    ;  ;       ;    ;    ;       ;    
   ;     ;     ;   ;;  ;    ;   ;   ;  ;   ;;    ;       ;    
   ;     ;      ;;; ;  ;    ;    ;;;    ;;; ;     ;;;     ;;; 
   ;                                                          
   ;                                                          
   ;                                                          
   ; Function call
   [--> (σ_1 : (osp_1 ...
                (cid (function Name_1 (Name_2 ...) s_1 end))
                osp_2 ...) : (cid (v ...)))
        
        (σ_2 : (osp_1 ...
                (cid (function Name_1 (Name_2 ...) s_1 end))
                osp_2 ...) : ((substBlock s_1 ((id e) ...)) (renv ...) RetExp))

        E-ApplyExp   

        ; Length of the formal parameters list
        (where Number_1 ,(length (term (Name_2 ...))))

        ; Length of the actual parameters list
        (where Number_2 ,(length (term (v ...))))

        ; Take the corresponding amount of actual parameters
        (where (v_1 ...) ,(take (term (v ...))
                                (min (term Number_1)
                                     (term Number_2))))

        (where Number_3 ,(- (term Number_1) (term Number_2)))

        ; Calculate the amount of extra nil values
        (where Number_4 ,(if (>= (term Number_3) 0)
                             (term Number_3)
                             0))
        
        ; Add nil values, if needed
        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (term Number_4)
                                             (term nil))))

        ; Add, to the value store, mappings to the actual paramaters
        (where (σ_2 (r ...)) (addSimpVal σ_1 (v_2 ...)))

        ; Create the substitution mapping that will replace formal
        ; parameters' identifiers by the corresponding references
        (where ((id e) ...) ,(map (lambda (id ref) (list id ref))
                                  (term (Name_2 ...))
                                  (term (r ...))))

        ; Tag the references to convert them into renv
        (where (renv ...) ,(map (lambda (r) (term (rEnv (unquote r))))
                                (term (r ...))))
        ]

    [--> (σ_1 : (osp_1 ...
                (cid (function Name_1 (Name_2 ...) s_1 end))
                osp_2 ...) : ($statFunCall cid (v ...)))
         
        (σ_2 : (osp_1 ...
                (cid (function Name_1 (Name_2 ...) s_1 end))
                osp_2 ...) : ((substBlock s_1 ((id e) ...)) (renv ...) RetStat))

        E-ApplyStat              

        ; Length of the formal parameters list
        (where Number_1 ,(length (term (Name_2 ...))))

        ; Length of the actual parameters list
        (where Number_2 ,(length (term (v ...))))

        ; Take the corresponding amount of actual parameters
        (where (v_1 ...) ,(take (term (v ...))
                                (min (term Number_1)
                                     (term Number_2))))

        (where Number_3 ,(- (term Number_1) (term Number_2)))

        ; Calculate the amount of extra nil values
        (where Number_4 ,(if (>= (term Number_3) 0)
                             (term Number_3)
                             0))
        
        ; Add nil values, if needed
        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (term Number_4)
                                             (term nil))))

        ; Add, to the value store, mappings to the actual paramaters
        (where (σ_2 (r ...)) (addSimpVal σ_1 (v_2 ...)))

        ; Create the substitution mapping that will replace formal
        ; parameters' identifiers by the corresponding references
        (where ((id e) ...) ,(map (lambda (id ref) (list id ref))
                                  (term (Name_2 ...))
                                  (term (r ...))))

        ; Tag the references to convert them into renv
        (where (renv ...) ,(map (lambda (r) (term (rEnv (unquote r))))
                                (term (r ...))))
        ]

   ; Vararg function call
   [--> (σ_1 : (osp_1 ...
                (cid (function Name_1 (Name_2 ... <<<) s_1 end))
                osp_2 ...) : (cid (v ...)))
        
        (σ_2 : (osp_1 ...
                (cid (function Name_1 (Name_2 ... <<<) s_1 end))
                osp_2 ...) : ((substBlock s_1 ((id_1 e_1) ...
                                               (<<< (< v_3 ... >))))
                              (renv ...) RetExp))

        E-ApplyVararg

        ; Length of the formal parameters list
        (where Number_1 ,(length (term (Name_2 ...))))

        ; Length of the actual parameters list
        (where Number_2 ,(length (term (v ...))))

        ; Take the corresponding amount of actual parameters
        (where (v_1 ...) ,(take (term (v ...))
                                (min (term Number_1)
                                     (term Number_2))))

        (where Number_3 ,(- (term Number_1) (term Number_2)))

        ; Calculate the amount of extra nil values
        (where Number_4 ,(if (>= (term Number_3) 0)
                             (term Number_3)
                             0))

        ; Take the extra-arguments, if present
        (where Number_5 ,(if (= (term Number_4) 0)
                             (- (term Number_2) (term Number_1))
                             0))
        
        ; Add nil values, if needed
        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (term Number_4)
                                             (term nil))))

        ; (v_3 ...) are the values that go wrapped into the tuple
        (where (v_3 ...) ,(take-right (term (v ...)) (term Number_5)))

        ; Add, to the value store, mappings to the actual paramaters
        (where (σ_2 (r ...)) (addSimpVal σ_1 (v_2 ...)))

        ; Create the substitution mapping that will replace formal
        ; parameters' identifiers by the corresponding references
        (where ((id_1 e_1) ...) ,(map (lambda (id ref) (list id ref))
                                      (term (Name_2 ...))
                                      (term (r ...))))

        ; Tag the references to convert them into renv
        (where (renv ...) ,(map (lambda (r) (term (rEnv (unquote r))))
                                (term (r ...))))]

   [--> (σ_1 : (osp_1 ...
                (cid (function Name_1 (Name_2 ... <<<) s_1 end))
                osp_2 ...) : ($statFunCall cid (v ...)))
        
        (σ_2 : (osp_1 ...
                (cid (function Name_1 (Name_2 ... <<<) s_1 end))
                osp_2 ...) : ((substBlock s_1 ((id_1 e_1) ...
                                               (<<< (< v_3 ... >))))
                              (renv ...) RetStat))

        E-ApplyVarargStat

        ; Length of the formal parameters list
        (where Number_1 ,(length (term (Name_2 ...))))

        ; Length of the actual parameters list
        (where Number_2 ,(length (term (v ...))))

        ; Take the corresponding amount of actual parameters
        (where (v_1 ...) ,(take (term (v ...))
                                (min (term Number_1)
                                     (term Number_2))))

        (where Number_3 ,(- (term Number_1) (term Number_2)))

        ; Calculate the amount of extra nil values
        (where Number_4 ,(if (>= (term Number_3) 0)
                             (term Number_3)
                             0))

        ; Take the extra-arguments, if present
        (where Number_5 ,(if (= (term Number_4) 0)
                             (- (term Number_2) (term Number_1))
                             0))
        
        ; Add nil values, if needed
        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (term Number_4)
                                             (term nil))))

        ; (v_3 ...) are the values that go wrapped into the tuple
        (where (v_3 ...) ,(take-right (term (v ...)) (term Number_5)))

        ; Add, to the value store, mappings to the actual paramaters
        (where (σ_2 (r ...)) (addSimpVal σ_1 (v_2 ...)))

        ; Create the substitution mapping that will replace formal
        ; parameters' identifiers by the corresponding references
        (where ((id_1 e_1) ...) ,(map (lambda (id ref) (list id ref))
                                      (term (Name_2 ...))
                                      (term (r ...))))

        ; Tag the references to convert them into renv
        (where (renv ...) ,(map (lambda (r) (term (rEnv (unquote r))))
                                (term (r ...))))]

   
   ; built-in services
   [--> (σ_1 : θ : ($builtIn print (v ...)))
        (σ_2 : θ : nil)
        
        E-BuiltInSigmaWrite
        
        (where σ_2 (δ (print v ... σ_1 θ)))
        ]
   ))
(provide terms-val-obj-store)
