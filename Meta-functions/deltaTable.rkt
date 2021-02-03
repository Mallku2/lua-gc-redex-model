#lang racket
(require redex
         "../grammar.rkt"
         "./objStoreMetaFunctions.rkt"
         "./deltaBasic.rkt")


; We define the semantics of the binary and unary operators of our language
; in terms of operations of PLT Racket. The "," symbol is treated as an escape
; to PLT Racket code. So, in general, the semantics of an expression
; (◇ op_1 op_2) is defined as the PLT Racket code (◇ (term op_1) (term op_2))
; when ◇ is also an operator of PLT Racket.
(define-metafunction ext-lang
  ;                                          
  ;                   ;       ;;;            
  ;                   ;         ;            
  ;     ;             ;         ;            
  ;     ;             ;         ;            
  ;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
  ;     ;      ;   ;  ;;  ;;    ;     ;;  ;; 
  ;     ;          ;  ;    ;    ;     ;    ; 
  ;     ;      ;;;;;  ;    ;    ;     ;;;;;; 
  ;     ;     ;    ;  ;    ;    ;     ;      
  ;     ;     ;   ;;  ;;  ;;    ;     ;;   ; 
  ;      ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
  ;                                          
  ;                                          
  ;                                          
  
  ;                                                  
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;     ;;;    ;;;;   ; ;;;     ;;;     ;;;   ;;;;;; 
  ;    ;   ;  ;;  ;;  ;;   ;   ;   ;   ;   ;    ;    
  ;   ;       ;    ;  ;    ;  ;            ;    ;    
  ;   ;       ;    ;  ;    ;  ;        ;;;;;    ;    
  ;   ;       ;    ;  ;    ;  ;       ;    ;    ;    
  ;    ;   ;  ;;  ;;  ;    ;   ;   ;  ;   ;;    ;    
  ;     ;;;    ;;;;   ;    ;    ;;;    ;;; ;     ;;; 
  ;                                                  
  ;
  [(δtable table.concat tid v_1 v_2 v_3 v_4 v_5 ... θ)
   (δtable table.concat tid v_1 v_2 v_3 θ)]
  
  ; missing parameters
  [(δtable table.concat tid θ)
   (δtable table.concat tid "" θ)]

  [(δtable table.concat tid String θ)
   (δtable table.concat tid String 1 θ)]

  ; default value for j is #tid
  [(δtable table.concat tid_1 String Number_1
           ((tid_2 object_2) ...
            (tid_1 (evaluatedtable any_1 any_2))
            (tid_3 object_3) ...))
   (δtable table.concat tid_1 String Number_1 (δbasic \# evaluatedtable)
           ((tid_2 object_2) ...
            (tid_1 (evaluatedtable any_1 any_2))
            (tid_3 object_3) ...))]

  ; last check
  [(δtable table.concat tid_1 v_1 v_2 v_3 ((tid_2 object_2) ...
                                           (tid_1 (evaluatedtable any_1 any_2))
                                           (tid_3 object_3) ...))
   (δtable table.concat tid_1 v_4 v_5 v_6 ((tid_2 object_2) ...
                                           (tid_1 (evaluatedtable any_1 any_2))
                                           (tid_3 object_3) ...))

   (side-condition (or (is_nil? (term v_1))
                       (is_nil? (term v_2))
                       (is_nil? (term v_3))))

   (where v_4 ,(if (is_nil? (term v_1))
                   ""
                   (term v_1)))

   (where v_5 ,(if (is_nil? (term v_2))
                   1
                   (term v_2)))

   (where v_6 ,(if (is_nil? (term v_3))
                   (term (δbasic \# evaluatedtable))
                   (term v_3)))]
  
  ; simpler case
  [(δtable table.concat tid String Number_1 Number_2 θ)
   any_3

   ; quantity of fields to be accessed
   (where Number_3 ,(+ (- (term Number_2) (term Number_1))
                       1))

   (side-condition (> (term Number_3)
                      0))

   ; construct a list of indexing operations, with numeric indices from
   ; Number_1 to Number_2
   (where (any_1 any_2 ...)
          ,(build-list (inexact->exact (term Number_3))
                       (λ (nmbr) (term (tid \[ ,(+ nmbr (term Number_1)) \])))))

   ; apply string concatenation between each field, separated by String
   (where any_3 ,(foldl (λ (field accum) (term (,accum .. (String .. ,field))))
                        (term any_1)
                        (term (any_2 ...))))]

  ; default case
  [(δtable table.concat tid String Number_1 Number_2 θ)
   ""]
  
  ;                                                  
  ;     ;                                            
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;   ;;;     ; ;;;    ;;;;    ;;;;    ;;;;   ;;;;;; 
  ;     ;     ;;   ;  ;    ;  ;;  ;;   ;;  ;    ;    
  ;     ;     ;    ;  ;       ;    ;   ;        ;    
  ;     ;     ;    ;   ;;;;   ;;;;;;   ;        ;    
  ;     ;     ;    ;       ;  ;        ;        ;    
  ;     ;     ;    ;  ;    ;  ;;   ;   ;        ;    
  ;   ;;;;;   ;    ;   ;;;;    ;;;;    ;         ;;; 
  ;                                                  
  ;                                                  
  ;
  [(δtable table.insert tid v_1 v_2 v_3 v_4 ... θ)
   (δtable table.insert tid v_1 v_2 θ)]

  ; default value for pos is #list + 1
  [(δtable table.insert tid_1 v ((tid_2 object_2) ...
                                 (tid_1 (evaluatedtable any_1 any_2))
                                 (tid_3 object_3) ...))
   (δtable table.concat tid_1 (δbasic + 1 (δbasic \# evaluatedtable)) v
           ((tid_2 object_2) ...
            (tid_1 (evaluatedtable any_1 any_2))
            (tid_3 object_3) ...))]

  ; special case
  [(δtable table.insert tid Number nil θ)
   (θ (< >))]
  
;  ; no pos provided: insert v at the end of tid
;  [(δtable table.insert tid v
;                    (osp_1 ...
;                     (tid ((\{ field ... \}) any ...))
;                     osp_2 ...))
;
;   ((osp_1 ...
;     (tid ((\{ field ... (\[  Number \] = v) \}) any ...))
;     osp_2 ...) (< >))
;
;   ; default value for pos is #list+1
;   (where Number (δbasic + 1 (δbasic \# (\{ field ... \}))))]

  ; pos provided
  [(δtable table.insert tid Number_1 v_1 (osp_1 ...
                                          (tid ((\{ efield_1 ... \}) any ...))
                                          osp_2 ...))

   ((osp_1 ...
     (tid ((\{ efield_2 ...
               ; value inserted
               (\[ Number_1 \] = v_1) 
               ; fields in list[pos], list[pos+1], ···, list[#list]
               (\[ Number_3 \] = v_5) ... 
               \}) any ...))
     osp_2 ...) (< >))

   ; obtain list length
   (where Number_2 (δbasic \# (\{ efield_1 ... \})))
   
   ; extract fields in tid[Number_1: Number_2]
   (where ((\[ v_4 \] = v_5) ...)
    ,(filter (lambda (field)
                     (redex-match? ext-lang
                                   (side-condition (|[| v_2 |]| = v_3)
                                                   (and (is_number? (term v_2))
                                                        (= (floor (term v_2))
                                                           (term v_2))
                                                        (>= (term v_2)
                                                            (term Number_1))
                                                        (<= (term v_2)
                                                            (term Number_2))))
                                   (term ,field)))
                   (term (efield_1 ...))))

   ; extract remaining fields
   (where (efield_2 ...)
    ,(filter (lambda (field)
                     (redex-match? ext-lang
                                   (side-condition (|[| v_2 |]| = v_3)
                                                   (not (and (is_number? (term v_2))
                                                             (= (floor (term v_2))
                                                                (term v_2))
                                                             (>= (term v_2)
                                                                 (term Number_1))
                                                             (<= (term v_2)
                                                                 (term Number_2)))))
                                   (term ,field)))
                   (term (efield_1 ...))))

   ; list of new numeric keys: v_1
   (where (Number_3 ...) ,(build-list (length (term ((\[ v_4 \] = v_5) ...)))
                                      (lambda (nmbr) (+ nmbr
                                                        (+ 1 (term Number_1))))))]
  ;                                  
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;   ;;;;;     ;;;     ;;;   ;   ;  
  ;   ;;  ;;   ;   ;   ;   ;  ;  ;   
  ;   ;    ;       ;  ;       ; ;    
  ;   ;    ;   ;;;;;  ;       ;;;    
  ;   ;    ;  ;    ;  ;       ;  ;   
  ;   ;;  ;;  ;   ;;   ;   ;  ;   ;  
  ;   ;;;;;    ;;; ;    ;;;   ;    ; 
  ;   ;                              
  ;   ;                              
  ;   ;                              
  
  
  [(δtable table.pack v ...)
   any_3
   
   (where Number ,(length (term (v ...))))
   
   ; take the list of keys (naturals starting from 1),
   ; the list of values received, and construct
   ; table fields taking 2 elements, one from each list.
   (where any ,(map (λ (number value)
                      (append (term (\[ ))
                              (list number)
                              (term (\] = ))
                              (list value)))
                    ; build the list of keys
                    (build-list (term Number) (λ (nmbr) (+ nmbr 1)))
                    ; and pass the values
                    (term (v ...))))
   
   ; filter nil-valued fields
   (where ((\[ v_1 \] = v_2) ...) ,(filter (λ (field)
                                             (not (redex-match? ext-lang
                                                                (\[ v \] = nil)
                                                                field)))
                                           (term any)))
   
   ; add the parenthesis and the field "n"
   ; NOTE: it seems that the implementation counts even the nil-valued
   ; fields.
   (where any_3 (\{ (\[ v_1 \] = v_2) ... (\[ "n" \] = Number) \}))]
  
  
  ;                                                  
  ;                                           ;      
  ;                                           ;      
  ;                                           ;      
  ;                                           ;      
  ;   ;    ;  ; ;;;   ;;;;;     ;;;     ;;;   ;   ;  
  ;   ;    ;  ;;   ;  ;;  ;;   ;   ;   ;   ;  ;  ;   
  ;   ;    ;  ;    ;  ;    ;       ;  ;       ; ;    
  ;   ;    ;  ;    ;  ;    ;   ;;;;;  ;       ;;;    
  ;   ;    ;  ;    ;  ;    ;  ;    ;  ;       ;  ;   
  ;   ;   ;;  ;    ;  ;;  ;;  ;   ;;   ;   ;  ;   ;  
  ;    ;;; ;  ;    ;  ;;;;;    ;;; ;    ;;;   ;    ; 
  ;                   ;                              
  ;                   ;                              
  ;                   ;
  [(δtable table.unpack tid v_1 v_2 v_3 v_4 ... θ)
   (δtable table.unpack tid v_1 v_2 θ)]
  
  ; default values
  [(δtable table.unpack tid (osp_1 ...
                             (tid (evaluatedtable any ...))
                             osp_2 ...))
   (δtable table.unpack tid 1 (δbasic \# evaluatedtable)
           (osp_1 ...
            (tid (evaluatedtable any ...))
            osp_2 ...))]

  [(δtable table.unpack tid v (osp_1 ...
                                 (tid (evaluatedtable any ...))
                                 osp_2 ...))
   (δtable table.unpack tid v (δbasic \# evaluatedtable)
           (osp_1 ...
            (tid (evaluatedtable any ...))
            osp_2 ...))]

  ; last check
  [(δtable table.unpack tid v_1 v_2 (osp_1 ...
                                       (tid (evaluatedtable any ...))
                                       osp_2 ...))
   (δtable table.unpack tid Number_1 Number_2 (osp_1 ...
                                               (tid (evaluatedtable any ...))
                                               osp_2 ...))

   (side-condition (or (is_nil? (term v_1))
                       (is_nil? (term v_2))))

   (where Number_1 ,(if (is_nil? (term v_1))
                        1
                        (term v_1)))
   
   (where Number_2 ,(if (is_nil? (term v_2))
                        (term (δbasic \# evaluatedtable))
                        (term v_2)))]

  ; coercion
  [(δtable table.unpack tid_1 String v θ)
   (δtable table.unpack tid_1 Number v θ)

   (where Number (δbasic tonumber String ()))]

  [(δtable table.unpack tid_1 Number_1 String θ)
   (δtable table.unpack tid_1 Number_1 Number_2 θ)

   (where Number_2 (δbasic tonumber String ()))]

  ; normal case
  [(δtable table.unpack tid Number_1 Number_2 θ)
   any_2

   ; construct a tuple of table indexing expressions
   (where any_2 ,(append (term (< ))

                         (map (λ (index)
                                (append (term (tid \[ ))
                                        (term (,index))
                                        (term (\]))))
                              
                              (range (exact-floor (term Number_1))
                                     (+ (exact-floor (term Number_2)) 1)))

                         (term ( >))))]




  ; to capture the "no value" error for every builtinserv 
  [(δtable builtinserv v ...)
   (δbasic error any)

   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))]

  ; services that don't modify theta
  [(δtable builtinserv v ... θ)
   (δbasic error any)

   (side-condition (member (term builtinserv)
                           (term (; table
                                  table.concat
                                  table.unpack))))
   
   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))]

  ; services that modify theta
  [(δtable builtinserv v ... θ)
   (θ (δbasic error any))
   
   (where any ,(string-append "erroneous actual parameters to "
                              (symbol->string (term builtinserv))))]
  )
(provide δtable)