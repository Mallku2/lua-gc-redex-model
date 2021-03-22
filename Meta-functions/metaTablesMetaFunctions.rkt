#lang racket
(require redex
         "../grammar.rkt"
         "./grammarMetaFunctions.rkt"
         ; indexMetaTable, rawget, error, type
         "../Meta-functions/deltaBasic.rkt")


;                                                                          
;                                                                          
;                                                                          
;                ;;;                                     ;;;;     ;;;;     
;               ;                                           ;        ;     
;               ;                                           ;        ;     
;  ;       ;  ;;;;;;  ;     ;  ; ;;;;     ;;;      ;;;;     ;        ;     
;  ;       ;    ;     ;     ;  ;;   ;;   ;   ;    ;    ;    ;        ;     
;   ;  ;  ;     ;     ;     ;  ;     ;  ;              ;    ;        ;     
;   ;  ;  ;     ;     ;     ;  ;     ;  ;         ;;;;;;    ;        ;     
;   ; ; ; ;     ;     ;     ;  ;     ;  ;        ;;    ;    ;        ;     
;   ; ; ; ;     ;     ;     ;  ;     ;  ;        ;     ;    ;        ;     
;    ;   ;      ;     ;;   ;;  ;     ;   ;   ;   ;    ;;    ;        ;     
;    ;   ;      ;      ;;;; ;  ;     ;    ;;;     ;;;; ;     ;;;      ;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          

; implements the meta-table mechanism for WFunCall
(define-metafunction ext-lang

  ; break loop
  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall
                                                        tid_1 ...
                                                        tid_2
                                                        tid_3 ...)))
   ; TODO: check this
   (δbasic error "loop in call")

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__call" θ))

   (side-condition (not (is_false_cond? (term v_3))))]

  [(w_fun_call (θ : (($statFunCall ..._1 v_1 (v_2 ...)) WFunCall tid_1 ...)))
   (($statFunCall ..._1 v_3 (v_1 v_2 ...)) Meta tid_1 ... tid_2)

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__call" θ))

   (side-condition (not (is_false_cond? (term v_3))))]

  [(w_fun_call (θ : (($statFunCall ... v_1 (v_2 ...)) WFunCall tid ...)))
   (δbasic error String)
   
   (where String (errmessage WFunCall (δbasic type v_1)))]
  )

(provide w_fun_call)


;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                                ;       ;;;;                                                  
;                                ;               ;          ;                                                  
;                                ;               ;          ;                                                  
;   ; ;;;;     ;;;    ; ;;;;   ;;;;;;     ;;;;   ; ;;;      ;        ;;;               ;;;    ;;   ;;  ; ;;;   
;   ;;   ;;   ;   ;   ;;   ;;    ;       ;    ;  ;;   ;     ;       ;   ;             ;   ;    ;   ;   ;;   ;  
;   ;     ;  ;     ;  ;     ;    ;            ;  ;     ;    ;      ;     ;           ;     ;    ; ;    ;     ; 
;   ;     ;  ;     ;  ;     ;    ;       ;;;;;;  ;     ;    ;      ;     ;           ;     ;     ;     ;     ; 
;   ;     ;  ;     ;  ;     ;    ;      ;;    ;  ;     ;    ;      ;;;;;;;           ;;;;;;;     ;     ;     ; 
;   ;     ;  ;     ;  ;     ;    ;      ;     ;  ;     ;    ;      ;                 ;          ; ;    ;     ; 
;   ;     ;   ;   ;   ;     ;    ;      ;    ;;  ;;   ;     ;       ;    ;            ;    ;   ;   ;   ;;   ;  
;   ;     ;    ;;;    ;     ;     ;;;    ;;;; ;  ; ;;;       ;;;     ;;;;              ;;;;   ;;   ;;  ; ;;;   
;                                                                                                      ;       
;                                                                                                      ;       
;                                                                                                      ;       
;                                                                                                              

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable
                                      tid_1 ...
                                      tid_2
                                      tid_3 ... )))
   (δbasic error "loop in gettable")

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]
  
  ; function handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable tid_1 ...)))
   ((\( (cid (v_1 v_2)) \)) Meta tid_1 ... tid_2)

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where cid (δbasic rawget tid_2 "__index" θ))]

  ; table handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable tid_1 ...)))
   ((v_3 \[ v_2 \]) Meta tid_1 ... tid_2)
   
   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(non_table_e (θ : ((v_1 \[ v_2 \]) NonTable tid ...)))
   (δbasic error String)
   
   (where String (errmessage NonTable
                             (δbasic type v_1)))]
  )

(provide non_table_e)



;                                                                                                              
;                                                                                                              
;                                                                                                              
;                                                ;                                                             
;                                                ;                                                             
;                                                ;                                                             
;  ;       ;   ; ;;;    ;;;    ; ;;;;     ;;; ;  ;    ;     ;;;    ;     ;             ;;;    ;;   ;;  ; ;;;   
;  ;       ;   ;;   ;  ;   ;   ;;   ;;   ;   ;;  ;  ;;     ;   ;    ;   ;             ;   ;    ;   ;   ;;   ;  
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ; ;      ;     ;   ;   ;            ;     ;    ; ;    ;     ; 
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ;;;      ;     ;   ;   ;            ;     ;     ;     ;     ; 
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;  ;     ;;;;;;;    ; ;             ;;;;;;;     ;     ;     ; 
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;   ;    ;          ; ;             ;          ; ;    ;     ; 
;    ;   ;     ;       ;   ;   ;     ;   ;   ;;  ;    ;    ;    ;     ;;              ;    ;   ;   ;   ;;   ;  
;    ;   ;     ;        ;;;    ;     ;    ;;; ;  ;     ;    ;;;;      ;                ;;;;   ;;   ;;  ; ;;;   
;                                             ;                       ;                                ;       
;                                        ;   ;;                       ;                                ;       
;                                         ;;;;                      ;;                                 ;       
;                                                                                                              

; implements the meta-table mechanism for WrongKey
(define-metafunction ext-lang

  ; break loop
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey
                                      tid_1 ...
                                      tid_2
                                      tid_3 ... )))
   (δbasic error "loop in gettable")

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]

  ; function handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey tid_1 ... )))
   ((\( (cid (v_1 v_2)) \)) Meta tid_1 ... tid_2)

   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where cid (δbasic rawget tid_2 "__index" θ))]

  ; table handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey tid_1 ...)))
   ((v_3 \[ v_2 \]) Meta tid_1 ... tid_2)
   
   ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_3 (δbasic rawget tid_2 "__index" θ))

   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(wrong_key_e (θ : ((v_1 \[ v_2 \]) WrongKey objid ...)))
   nil]
  )

(provide wrong_key_e)


;                                                                 
;                                                                 
;                                                                 
;   ;           ;                                                 
;   ;           ;                                                 
;   ;                                                             
;   ; ;;;     ;;;     ; ;;;;     ;;;    ; ;;;   ;       ;   ;;;   
;   ;;   ;      ;     ;;   ;;   ;   ;   ;;   ;  ;       ;  ;   ;  
;   ;     ;     ;     ;     ;  ;     ;  ;     ;  ;  ;  ;  ;     ; 
;   ;     ;     ;     ;     ;  ;     ;  ;     ;  ;  ;  ;  ;     ; 
;   ;     ;     ;     ;     ;  ;     ;  ;     ;  ; ; ; ;  ;     ; 
;   ;     ;     ;     ;     ;  ;     ;  ;     ;  ; ; ; ;  ;     ; 
;   ;;   ;      ;     ;     ;   ;   ;   ;;   ;    ;   ;    ;   ;  
;   ; ;;;    ;;;;;;;  ;     ;    ;;;    ; ;;;     ;   ;     ;;;   
;                                       ;                         
;                                       ;                         
;                                       ;                         
;                                                                 
                                                                              

; implements the meta-table mechanism for BinopWO
(define-metafunction ext-lang

  ; break loop
  [(binop_wo (θ : ((v_1 binop v_2) BinopWO
                                   tid_1 ...
                                   tid_2
                                   tid_3 ...)))
   (δbasic error "loop in arithop")
        
   ; determine if v_1 or v_2 efectively have a meta-table
   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey binop)
                                     θ))]

  ; metatables for arith ops and .. have an analogous semantics
  [(binop_wo (θ : ((v_1 binop v_2) BinopWO tid_1 ...)))
   ((\( (v_3 (v_1 v_2)) \)) Meta tid_1 ... tid_2)

   (side-condition (or (is_strconcat? (term binop))
                       (is_arithop? (term binop))))
   
   ; determine if v_1 or v_2 efectively have a meta-table
   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey binop)
                                     θ))]

  ; handler for <
  [(binop_wo (θ : ((v_1 < v_2) BinopWO tid_1 ...)))
   ((not (not (v_3 (v_1 v_2)))) Meta tid_1 ... tid_2)

   ; obtain a handler for the operation
   (where (tid_2 v_3) (getBinHandler v_1 
                                     v_2 
                                     (binopeventkey <) 
                                     θ))]

  ; handler for <=
  [(binop_wo (θ : ((v_1 <= v_2) BinopWO tid_1 ...)))
   ((not (not (v_3 (v_1 v_2)))) Meta tid_1 ... tid_2)

   ; obtain a handler for the operation
   (where (tid_2 v_3) (getBinHandler v_1 
                                     v_2 
                                     (binopeventkey <=) 
                                     θ))]

  ; try with not (v_2 < v_1)
  [(binop_wo (θ : ((v_1 <= v_2) BinopWO tid_1 ...)))
  ((not (v_3 (v_2 v_1))) Meta tid_1 ... tid_2)

   (where (tid_2 v_3) (getBinHandler v_1
                                     v_2
                                     (binopeventkey <)
                                     θ))]

  ; no handler
  [(binop_wo (θ : ((v_1 binop v_2) BinopWO objid ...)))
   (δbasic error String)
        
   (where String (errmessage BinopWO
                             binop
                             (δbasic type v_1)
                             (δbasic type v_2)))]
  )

(provide binop_wo)

;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                                 
;                                                                 
;   ; ;;;;     ;;;      ;;; ; ;       ;   ;;;    ; ;;;     ;;;;;  
;   ;;   ;;   ;   ;    ;   ;; ;       ;  ;   ;   ;;   ;   ;     ; 
;   ;     ;  ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;       
;   ;     ;  ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;;;;    
;   ;     ;  ;;;;;;;  ;     ;  ; ; ; ;  ;     ;  ;     ;      ;;; 
;   ;     ;  ;        ;     ;  ; ; ; ;  ;     ;  ;     ;        ; 
;   ;     ;   ;    ;   ;   ;;   ;   ;    ;   ;   ;;   ;   ;     ; 
;   ;     ;    ;;;;     ;;; ;   ;   ;     ;;;    ; ;;;     ;;;;;  
;                           ;                    ;                
;                      ;   ;;                    ;                
;                       ;;;;                     ;                
;                                                                 

; implements the meta-table mechanism for NegWrongOp
(define-metafunction ext-lang

  ; break loop
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp
                               tid_1 ...
                               tid_2
                               tid_3 ...)))
   ; TODO: check this
  (δbasic error "loop in negop")
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey -) 
                                       θ))]
  
  ; handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp tid_1 ...)))
   ((\( (v_2 (v_1)) \)) Meta tid_1 ... tid_2)
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey -) 
                                       θ))]

  ; no handler
  [(neg_wrong_op (θ : ((- v_1) NegWrongOp objid ...)))
  (δbasic error String)
   
   (where String (errmessage NegWrongOp (δbasic type v_1)))]
  )

(provide neg_wrong_op)


;                                                                                            
;                                                                                            
;                                                                                            
;                             ;;;;                                                           
;              ;                 ;                                                           
;              ;                 ;                                                           
;    ;;;;;   ;;;;;;     ; ;;;    ;        ;;;    ; ;;;;  ;       ;   ;;;    ; ;;;     ;;;;;  
;   ;     ;    ;        ;;   ;   ;       ;   ;   ;;   ;; ;       ;  ;   ;   ;;   ;   ;     ; 
;   ;          ;        ;        ;      ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;       
;   ;;;;       ;        ;        ;      ;     ;  ;     ;  ;  ;  ;  ;     ;  ;     ;  ;;;;    
;       ;;;    ;        ;        ;      ;;;;;;;  ;     ;  ; ; ; ;  ;     ;  ;     ;      ;;; 
;         ;    ;        ;        ;      ;        ;     ;  ; ; ; ;  ;     ;  ;     ;        ; 
;   ;     ;    ;        ;        ;       ;    ;  ;     ;   ;   ;    ;   ;   ;;   ;   ;     ; 
;    ;;;;;      ;;;     ;         ;;;     ;;;;   ;     ;   ;   ;     ;;;    ; ;;;     ;;;;;  
;                                                                           ;                
;                                                                           ;                
;                                                                           ;                
;                                                                                            

; implements the meta-table mechanism for StrLenWrongOp
(define-metafunction ext-lang

  ; break loop
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp
                                    tid_1 ...
                                    tid_2
                                    tid_3 ...)))
   ; TODO: check this
   (δbasic error "loop in str. len.")
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey \#) 
                                       θ))]
  
  ; handler
  [(str_len_wrong_op (θ : ((\# v_1) StrLenWrongOp tid_1 ...)))
   ((\( (v_2 (v_1)) \)) Meta tid_1 ...  tid_2)
        
   ; determine if v_1 efectively has a meta-table
   (where (tid_2 v_2) (getUnaryHandler v_1 
                                       (unopeventkey \#) 
                                       θ))]

  ; no handler: table length
  [(str_len_wrong_op (θ : ((\# tid_1) StrLenWrongOp tid_2 ...)))
   (δbasic \# evaluatedtable) ; no need for context Meta
   
   (where (osp_1 ... (tid_1 (evaluatedtable any ...)) osp_2 ...)
          θ)]

  ; no handler, no tid value
  [(str_len_wrong_op (θ : ((\# v) StrLenWrongOp tid ...)))
   (δbasic error String_2)
        
   (where String_1 (δbasic type v))
        
   (where String_2 (errmessage StrLenWrongOp String_1))]
  )

(provide str_len_wrong_op)


;                                                        
;                                                        
;                                                        
;                         ;;;              ;    ;;;;     
;                        ;                 ;       ;     
;                        ;                         ;     
;     ;;;      ;;; ;   ;;;;;;    ;;;;    ;;;       ;     
;    ;   ;    ;   ;;     ;      ;    ;     ;       ;     
;   ;     ;  ;     ;     ;           ;     ;       ;     
;   ;     ;  ;     ;     ;      ;;;;;;     ;       ;     
;   ;;;;;;;  ;     ;     ;     ;;    ;     ;       ;     
;   ;        ;     ;     ;     ;     ;     ;       ;     
;    ;    ;   ;   ;;     ;     ;    ;;     ;       ;     
;     ;;;;     ;;; ;     ;      ;;;; ;  ;;;;;;;     ;;;  
;                  ;                                     
;                  ;                                     
;                  ;                                     
;                                                        

; implements the meta-table mechanism for EqFail
(define-metafunction ext-lang

  ; break loop
  [(eq_fail (θ : ((v_1 == v_2) EqFail
                               tid_1 ...
                               tid_2
                               tid_3 ...)))
   ; TODO: check this
   (δbasic error "loop in eq.")
        
   (where (tid_4 ... tid_2 tid_5 ... v_3) (getEqualHandler v_1 v_2 θ))]

  ; handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail tid_1 ...)))
   ((not (not (v_3 (v_1 v_2)))) Meta tid_1 ... tid_2 ...)

   ; we need to access to 1 or 2 meta-tables
   (where (tid_2 ... v_3) (getEqualHandler v_1 v_2 θ))]

  ; no handler
  [(eq_fail (θ : ((v_1 == v_2) EqFail tid ...)))
   false]
  )

(provide eq_fail)



;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;                                                ;       ;;;;                                                           
;                                ;               ;          ;                                   ;                 ;     
;                                ;               ;          ;                                   ;                 ;     
;   ; ;;;;     ;;;    ; ;;;;   ;;;;;;     ;;;;   ; ;;;      ;        ;;;              ;;;;;   ;;;;;;     ;;;;   ;;;;;;  
;   ;;   ;;   ;   ;   ;;   ;;    ;       ;    ;  ;;   ;     ;       ;   ;            ;     ;    ;       ;    ;    ;     
;   ;     ;  ;     ;  ;     ;    ;            ;  ;     ;    ;      ;     ;           ;          ;            ;    ;     
;   ;     ;  ;     ;  ;     ;    ;       ;;;;;;  ;     ;    ;      ;     ;           ;;;;       ;       ;;;;;;    ;     
;   ;     ;  ;     ;  ;     ;    ;      ;;    ;  ;     ;    ;      ;;;;;;;               ;;;    ;      ;;    ;    ;     
;   ;     ;  ;     ;  ;     ;    ;      ;     ;  ;     ;    ;      ;                       ;    ;      ;     ;    ;     
;   ;     ;   ;   ;   ;     ;    ;      ;    ;;  ;;   ;     ;       ;    ;           ;     ;    ;      ;    ;;    ;     
;   ;     ;    ;;;    ;     ;     ;;;    ;;;; ;  ; ;;;       ;;;     ;;;;             ;;;;;      ;;;    ;;;; ;     ;;;  
;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;                                                                                                                       

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable
                                              tid_1 ...
                                              tid_2
                                              tid_3 ...)))
   ; TODO: check this
   (δbasic error "loop in settable")
   
    ; determine if v_1 has a meta-table
   (where tid_2 (getMetaTable v_1 θ))
   (where v_4 (δbasic rawget tid_2 "__newindex" θ))

    (side-condition (not (is_nil? (term v_4))))]
  
  ; function handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable tid_1 ...)))
  (($statFunCall cid (v_1 v_2 v_3)) Meta tid_1 ... tid_2)

   ; determine if v_1 has a meta-table with a handler
   (where tid_2 (getMetaTable v_1 θ))
   (where cid (δbasic rawget tid_2 "__newindex" θ))]

  ; table handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable tid_1 ...)))
  (((v_4 \[ v_2 \]) = v_3) Meta tid_1 ... tid_2)
        
   ; determine if v_1 has a meta-table with a handler
   (where tid_2 (getMetaTable v_1 θ))
   (where v_4 (δbasic rawget tid_2 "__newindex" θ))

   (side-condition (not (is_nil? (term v_4))))]

  ; no handler
  [(non_table_s (θ : (((v_1 \[ v_2 \]) = v_3) NonTable tid ...)))
   (δbasic error String)

   (where String ,(string-append "attempt to index a "
                                 (term (δbasic type v_1))
                                 " value"))]
  )

(provide non_table_s)


;                                                                                                                       
;                                                                                                                       
;                                                                                                                       
;                                                ;                                                                      
;                                                ;                                              ;                 ;     
;                                                ;                                              ;                 ;     
;  ;       ;   ; ;;;    ;;;    ; ;;;;     ;;; ;  ;    ;     ;;;    ;     ;            ;;;;;   ;;;;;;     ;;;;   ;;;;;;  
;  ;       ;   ;;   ;  ;   ;   ;;   ;;   ;   ;;  ;  ;;     ;   ;    ;   ;            ;     ;    ;       ;    ;    ;     
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ; ;      ;     ;   ;   ;            ;          ;            ;    ;     
;   ;  ;  ;    ;      ;     ;  ;     ;  ;     ;  ;;;      ;     ;   ;   ;            ;;;;       ;       ;;;;;;    ;     
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;  ;     ;;;;;;;    ; ;                 ;;;    ;      ;;    ;    ;     
;   ; ; ; ;    ;      ;     ;  ;     ;  ;     ;  ;   ;    ;          ; ;                   ;    ;      ;     ;    ;     
;    ;   ;     ;       ;   ;   ;     ;   ;   ;;  ;    ;    ;    ;     ;;             ;     ;    ;      ;    ;;    ;     
;    ;   ;     ;        ;;;    ;     ;    ;;; ;  ;     ;    ;;;;      ;               ;;;;;      ;;;    ;;;; ;     ;;;  
;                                             ;                       ;                                                 
;                                        ;   ;;                       ;                                                 
;                                         ;;;;                      ;;                                                  
;                                                                                                                       

; implements the meta-table mechanism for NonTable
(define-metafunction ext-lang

  ; break loop
  [(wrong_key_s (θ : (((tid_1 \[ v_1 \]) = v_2) WrongKey
                                              tid_2 ...
                                              tid_3
                                              tid_4 ...)))
   ; TODO: check this
   (θ : (δbasic error "loop in settable"))

   ; determine if v_1 has a meta-table with a handler
   (where tid_3 (getMetaTable tid_1 θ))
   (where v_3 (δbasic rawget tid_3 "__newindex" θ))

   (side-condition (not (is_nil? (term v_3))))]
  
  ; function handler
  [(wrong_key_s (θ : (((tid_1 \[ v_1 \]) = v_2) WrongKey tid_2 ...)))
   (θ : (($statFunCall cid (tid_1 v_1 v_2)) Meta tid_2 ... tid_3))

   ; determine if v_1 has a meta-table with a handler
   (where tid_3 (getMetaTable tid_1 θ))
   (where cid (δbasic rawget tid_3 "__newindex" θ))]

  ; table handler
  [(wrong_key_s (θ : (((tid_1 \[ v_1 \]) = v_2) WrongKey tid_2 ...)))
   (θ : (((v_3 \[ v_1 \]) = v_2) Meta tid_2 ... tid_3))
        
   ; determine if v_1 has a meta-table with a handler
   (where tid_3 (getMetaTable tid_1 θ))
   (where v_3 (δbasic rawget tid_3 "__newindex" θ))
   
   (side-condition (not (is_nil? (term v_3))))]

  ; no handler
  [(wrong_key_s (θ_1 : (((tid_1 \[ v_1 \]) = v_2) WrongKey tid_2 ...)))
   (θ_2 : any_2)

   ; try to create new field [v_1] = v_2
   (where (θ_2 any_1) (δbasic rawset tid_1 v_1 v_2 θ_1))

   ; any_2 should be either skip (rawset was successful) or $err v
   ; in this way we simplify rules
   (where any_2 ,(if (is_tid? (term any_1))
                     (term \;) ; success
                     (term any_1) ; any_1 is an error object: key was
                     ; nil,nan
                     ))]
  )

(provide wrong_key_s)
