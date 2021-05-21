#lang racket
(require redex
         "../grammar.rkt"
         "./grammarMetaFunctions.rkt"
         "./deltaBasic.rkt")

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
  [(δtable table.concat tid v_1 v_2 v_3 v_4 v_5 ...)
   (δtable table.concat tid v_1 v_2 v_3)]
  
  ; missing parameters
  [(δtable table.concat tid)
   (δtable table.concat tid "")]

  [(δtable table.concat tid v)
   (δtable table.concat tid v 1)]

  ; default value for j is #tid
  ; internally, table length is computed without resorting to just rawlen:
  ; its semantics involves the whole complexity of Lua's # op
  [(δtable table.concat tid v_1 v_2)
   (δtable table.concat tid v_1 v_2 (\# tid))]

  ; last check
   [(δtable table.concat tid v_1 v_2 v_3)
   (δtable table.concat tid v_4 v_5 e)

   (side-condition (or (is_nil? (term v_1))
                       (is_nil? (term v_2))
                       (is_nil? (term v_3))))

   (where v_4 ,(if (is_nil? (term v_1))
                   ""
                   (term v_1)))

   (where v_5 ,(if (is_nil? (term v_2))
                   1
                   (term v_2)))

   (where e ,(if (is_nil? (term v_3))
                 ; internally, table length is computed without resorting to
                 ; just rawlen: its semantics involves the whole complexity of
                 ; Lua's # op
                 (term (\# tid))
                 (term v_3)))]
  
  ; simpler case
  [(δtable table.concat tid String Number e)
   ((\( (function $dummy ()
                  (local i j accum value = Number e "" nil in
                    ((if (i <= j) then
                         ((value = (tid \[ i \]))
                          (if ((($builtIn type (value)) == "number")
                               or
                               (($builtIn type (value)) == "string"))
                              then
                              (accum = value)
                              else
                              (return ($builtIn error
                                                ((("invalid value at index "
                                                   .. ($builtIn tostring (i)))
                                                  .. " in table for 'concat'"))))
                              end)
                          (i = (i + 1))
                          (while (i <= j) do
                                 ((value = (tid \[ i \]))
                                  (if ((($builtIn type (value)) == "number")
                                       or
                                       (($builtIn type (value)) == "string"))
                                      then
                                      ((accum = (accum .. String))
                                       (accum = (accum .. value)))
                                      else
                                      (return ($builtIn error
                                                        ((("invalid value at index "
                                                           .. ($builtIn tostring (i)))
                                                          .. " in table for 'concat'"))))
                                      end)
                                  (i = (i + 1)))
                                 end))
                         ; {i > j}
                         else
                         \;
                         end)
                     (return accum))
                    end)
                  end) \)) ())]

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
  [(δtable table.insert tid v_1 v_2 v_3 v_4 ...)
   (δtable table.insert tid v_1 v_2)]

  ; default value for pos is #list + 1
  [(δtable table.insert tid v)
   (δtable table.insert tid ((\# tid) + 1) v)]

  ; special cases
  [(δtable table.insert tid Number nil)
   (< >)]

  ; pos provided
  [(δtable table.insert tid e v)
   ((\( (function $dummy ()
                  (local pos len = e (\# tid) in
                    (if ((pos <= 0) or (pos > (1 + len))) then
                        (return ($builtIn error ("bad argument #2 to 'insert' (position out of bounds)")))
                        else
                        ; {pos <= (1 + len)}
                        (local vpos aux = (tid \[ pos \]) nil in
                          ; assign v to pos
                          (
                           ((tid \[ pos \]) = v)
                           (pos = (pos + 1))
                           ; shift values from pos to #tid
                           ; {vpos = tid [ pos - 1 ]} 
                           (while (pos <= (1 + len)) do
                                  (
                                   (aux = (tid \[ pos \]))
                                   ((tid \[ pos \]) = vpos)
                                   (pos = (pos + 1))
                                   (vpos = aux)
                                   )
                                  end)
                           )
                          end)
                        end)
                    end)
                  end) \)) ())]
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

   ; limit check, something the actually is part of table.unpack's semantics,
   ; but added mostly for testing and performance reasons
   (side-condition (<= (+ (- (term Number_2) (term Number_1)) 1)
                       ; TODO: artificial limit
                       20))
 
   ; construct a tuple of table indexing expressions
   (where any_2 ,(append (term (< ))

                         (map (λ (index)
                                (append (term (tid \[ ))
                                        (term (,index))
                                        (term (\]))))
                              
                              (range (exact-floor (term Number_1))
                                     (+ (exact-floor (term Number_2)) 1)))

                         (term ( >))))]

  ; too many results to unpack
  [(δtable table.unpack tid Number_1 Number_2 θ)
   (δbasic error "too many results to unpack")]




  ; to capture the "no value" error for every builtinserv 
  [(δtable builtinserv any ...)
   (δbasic error String)

   (side-condition (member (term builtinserv)
                           (term (table.concat
                                  table.insert
                                  table.pack
                                  table.unpack))))
   
   (where String ,(string-append "erroneous actual parameters to "
                                 (symbol->string (term builtinserv))))]
  )
(provide δtable)