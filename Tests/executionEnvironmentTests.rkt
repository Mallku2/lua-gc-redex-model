#lang racket
(require redex
         "../grammar.rkt"
         "../executionEnvironment.rkt"
         "../Relations/fullProgs.rkt"
         "../Meta-functions/objStoreMetaFunctions.rkt")

; "black-box testing"

(define (standard-library-test-suite)
  ; calling services with incorrect amount of actual parameters
  (test-->> full-progs-rel
            (plugIntoExecutionEnvironment
             services
             '("type")
             (term ($statFCall (_ENV \[ "type" \]) ())))
            
            (term
             ((((ref 1) (objr ,objStoreFirstLocation)) ((ref 2) nil))
              :
              (((objr ,objStoreFirstLocation)
                ((|{|
                  (|[| "_G" |]| = (objr ,objStoreFirstLocation))
                  (|[| "type" |]| = (cl 7))
                  |}|)
                 nil
                 ⊥))
               ((cl 7)
                (function
                 $type
                 (<<<)
                 (return ($builtIn type (<<<)))
                 end)))
              :
              ($err "erroneous actual parameters to type"))))

  (test-->> full-progs-rel
            (plugIntoExecutionEnvironment
             services
             '("assert")
             (term ($statFCall (_ENV \[ "assert" \]) ())))
            
            (term
             ((((ref 1) (objr ,objStoreFirstLocation)) ((ref 2) nil))
              :
              (((objr ,objStoreFirstLocation)
                ((|{|
                  (|[| "_G" |]| = (objr ,objStoreFirstLocation))
                  (|[| "assert" |]| = (cl 7))
                  |}|)
                 nil
                 ⊥))
               ((cl 7)
                (function
                 $assert
                 (<<<)
                 (return ($builtIn assert (<<<)))
                 end)))
              :
              ($err "assertion failed!"))))

  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;     ;;;    ;;;;    ;;;;    ;;;;    ;;;;   ;;;;;; 
  ;    ;   ;  ;    ;  ;    ;  ;;  ;;   ;;  ;    ;    
  ;        ;  ;       ;       ;    ;   ;        ;    
  ;    ;;;;;   ;;;;    ;;;;   ;;;;;;   ;        ;    
  ;   ;    ;       ;       ;  ;        ;        ;    
  ;   ;   ;;  ;    ;  ;    ;  ;;   ;   ;        ;    
  ;    ;;; ;   ;;;;    ;;;;    ;;;;    ;         ;;; 
  ;                                                  
  ;                                                  
  ;                                                  
        
          
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "pcall")
     (term ((local status errmessage = ((_ENV \[ "pcall" \]) ((_ENV \[ "assert" \]) false "error message"))
              in
              (($statFCall (_ENV \[ "assert" \]) ((status == false)))
               ($statFCall (_ENV \[ "assert" \])
                             ((errmessage == "error message"))))
              end)
             
            ($statFCall (_ENV \[ "assert" \]) (true "error message"))
            ($statFCall (_ENV \[ "assert" \]) (1 2 3 4 5)))))))
          
          
          
            
  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;                                          
  ;    ;;;;    ;;;;    ;;;;    ;;;;    ;;;;  
  ;   ;;  ;;   ;;  ;   ;;  ;  ;;  ;;   ;;  ; 
  ;   ;    ;   ;       ;      ;    ;   ;     
  ;   ;;;;;;   ;       ;      ;    ;   ;     
  ;   ;        ;       ;      ;    ;   ;     
  ;   ;;   ;   ;       ;      ;;  ;;   ;     
  ;    ;;;;    ;       ;       ;;;;    ;     
  ;                                          
  ;                                          
  ;                                          
        
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "pcall" "error")
     (term (local status errmessage = ((_ENV \[ "pcall" \])
                                        ((_ENV \[ "error" \])
                                         "error message"))
              in
              (($statFCall (_ENV \[ "assert" \]) ((status == false)))
               ($statFCall (_ENV \[ "assert" \])
                             ((errmessage == "error message"))))
              end)))))
            
            
            
  ;                                                                                                  
  ;                                                                           ;       ;;;            
  ;                                                                           ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;    ;;;;;   ;;;;   ;;;;;;  ;;;;;;;  ;;;;   ;;;;;;    ;;;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
  ;   ;;  ;;  ;;  ;;    ;     ;  ;  ; ;;  ;;    ;      ;   ;    ;      ;   ;  ;;  ;;    ;     ;;  ;; 
  ;   ;    ;  ;    ;    ;     ;  ;  ; ;    ;    ;          ;    ;          ;  ;    ;    ;     ;    ; 
  ;   ;    ;  ;;;;;;    ;     ;  ;  ; ;;;;;;    ;      ;;;;;    ;      ;;;;;  ;    ;    ;     ;;;;;; 
  ;   ;    ;  ;         ;     ;  ;  ; ;         ;     ;    ;    ;     ;    ;  ;    ;    ;     ;      
  ;   ;;  ;;  ;;   ;    ;     ;  ;  ; ;;   ;    ;     ;   ;;    ;     ;   ;;  ;;  ;;    ;     ;;   ; 
  ;    ;;; ;   ;;;;      ;;;  ;  ;  ;  ;;;;      ;;;   ;;; ;     ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
  ;        ;                                                                                         
  ;    ;   ;                                                                                         
  ;     ;;;                                                                                          
        
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "setmetatable" "getmetatable")
     (term (local a b = (\{ \}) (\{ \}) in
             (($statFCall (_ENV \[ "setmetatable" \]) (a b))
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "getmetatable" \]) (a)) == b))))
             end)))))
        
        
                                                              
  ;     ;                       ;                    
  ;                                                  
  ;                                                  
  ;                                                  
  ;   ;;;     ;;;;;     ;;;   ;;;      ;;;;    ;;;;  
  ;     ;     ;;  ;;   ;   ;    ;      ;;  ;  ;    ; 
  ;     ;     ;    ;       ;    ;      ;      ;      
  ;     ;     ;    ;   ;;;;;    ;      ;       ;;;;  
  ;     ;     ;    ;  ;    ;    ;      ;           ; 
  ;     ;     ;;  ;;  ;   ;;    ;      ;      ;    ; 
  ;   ;;;;;   ;;;;;    ;;; ;  ;;;;;    ;       ;;;;  
  ;           ;                                      
  ;           ;                                      
  ;           ;                                      
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "ipairs" "setmetatable")
     (term ((local a = (\{ (\[ 1 \] = "a") (\[ 2 \] = "b") \}) in
              (local it table index = ((_ENV \[ "ipairs" \]) (a))
                in
                (($statFCall (_ENV \[ "assert" \]) ((table == a)))
                 ($statFCall (_ENV \[ "assert" \]) ((index == 0)))
                 (local nextIndex value = (it (a 0))
                   in
                   (($statFCall (_ENV \[ "assert" \]) ((nextIndex == 1)))
                                                             
                    ($statFCall (_ENV \[ "assert" \]) ((value == "a")))
                                                             
                    (nextIndex value = (it (a 1)))
                                                             
                    ($statFCall (_ENV \[ "assert" \]) ((nextIndex == 2)))
                                                             
                    ($statFCall (_ENV \[ "assert" \]) ((value == "b")))
                                                             
                    ; The iterator returned in successive calls
                    ; is the same
                    (local it2 table2 index2 = ((_ENV \[ "ipairs" \]) (a))
                      in
                                                               
                      ($statFCall (_ENV \[ "assert" \]) ((it2 == it)))
                                                               
                      end)
                    )
                                                            
                   end))
                end)
              end)
      
            (local a b = (\{ \}) (\{ (\[ "__ipairs" \] =
                                         (function $func1 ()
                                                   (return (< 1 2 3 >))
                                                   end)) \})
              in
              (($statFCall (_ENV \[ "setmetatable" \]) (a b))
               (local it table index = ((_ENV \[ "ipairs" \]) (a))
                 in
                 (($statFCall (_ENV \[ "assert" \]) ((it == 1)))
                  ($statFCall (_ENV \[ "assert" \]) ((table == 2)))
                  ($statFCall (_ENV \[ "assert" \]) ((index == 3)))
                                                           
                  ; The iterator returned in successive calls
                  ; is the same
                  (local it2 table2 index2 = ((_ENV \[ "ipairs" \]) (a))
                    in
                                                             
                    ($statFCall (_ENV \[ "assert" \]) ((it2 == it)))
                                                             
                    end))
                 end))
              end))))))
                                              
  ;                          ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;     ;      ;;;;     ;;;    ;;;;; 
  ;     ;     ;;  ;;   ;   ;  ;;  ;; 
  ;     ;     ;    ;       ;  ;    ; 
  ;     ;     ;    ;   ;;;;;  ;    ; 
  ;     ;     ;    ;  ;    ;  ;    ; 
  ;     ;     ;;  ;;  ;   ;;  ;;  ;; 
  ;      ;;;   ;;;;    ;;; ;   ;;;;; 
  ;                                  
  ;                                  
  ;                                  
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "load" "type")
     (term (((_ENV |[| "fat" |]|)
             =
             (function
              $2
              (x)
              (if (x <= 1) then (return (< 1 >))
                  else
                  (return (x * (((_ENV |[| "load" |]|)
                                 (("return fat(" .. ((x - 1) .. ")")))) ())))
                  end)
              end))
                   
            ($statFCall (_ENV \[ "assert" \]) (((_ENV \[ "load" \])
                                                  ("assert(fat(6)==720)"))))
            
            (local a = true in
              (local b = ((_ENV |[| "load" |]|)
                          ((function $1 ()
                                     (if a then
                                         ((a = false)
                                          (return "return true"))
                                         else
                                         (return nil)
                                         end)
                                     end)))
                in
                ($statFCall (_ENV \[ "assert" \]) ((b ())))
                end)
              end)
            
            (local proc = nil in
              ((proc = ((_ENV |[| "load" |]|)
                        ("assert(type == nil); return t(1)"
                         nil
                         nil
                         (\{ (\[ "t" \] = (_ENV |[| "type" |]|))
                             (\[ "assert" \] = (_ENV |[| "assert" |]|))\}))))
               ($statFCall (_ENV \[ "assert" \]) (((proc ()) == "number")))
            
               ($statFCall (_ENV \[ "assert" \])
                             ((((_ENV \[ "type" \]) (1)) == "number"))))
              end))))))
                               
                                                                              
  ;   ;;;                          ;     ;;     ;     ;;;            
  ;     ;                          ;    ;               ;            
  ;     ;                          ;    ;               ;            
  ;     ;                          ;    ;               ;            
  ;     ;      ;;;;     ;;;    ;;;;;  ;;;;;   ;;;       ;      ;;;;  
  ;     ;     ;;  ;;   ;   ;  ;;  ;;    ;       ;       ;     ;;  ;; 
  ;     ;     ;    ;       ;  ;    ;    ;       ;       ;     ;    ; 
  ;     ;     ;    ;   ;;;;;  ;    ;    ;       ;       ;     ;;;;;; 
  ;     ;     ;    ;  ;    ;  ;    ;    ;       ;       ;     ;      
  ;     ;     ;;  ;;  ;   ;;  ;;  ;;    ;       ;       ;     ;;   ; 
  ;      ;;;   ;;;;    ;;; ;   ;;;;;    ;     ;;;;;      ;;;   ;;;;  
  ;                                                                  
  ;                                                                  
  ;
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "loadfile" "load")
     (term (((_ENV \[ "fat" \]) = ((_ENV \[ "loadfile" \])
                                   ("loadfile_test.lua")))
            ($statFCall (_ENV \[ "assert" \])
                          ((((_ENV \[ "fat" \]) (1)) == 1)))
            ($statFCall (_ENV \[ "assert" \])
                          ((((_ENV \[ "fat" \]) (6)) == 720))))))))
            
  ;                                  
  ;                                  
  ;                                  
  ;                             ;    
  ;                             ;    
  ;   ; ;;;    ;;;;   ;;  ;;  ;;;;;; 
  ;   ;;   ;  ;;  ;;   ;  ;     ;    
  ;   ;    ;  ;    ;    ;;      ;    
  ;   ;    ;  ;;;;;;    ;;      ;    
  ;   ;    ;  ;         ;;      ;    
  ;   ;    ;  ;;   ;   ;  ;     ;    
  ;   ;    ;   ;;;;   ;;  ;;     ;;; 
  ;                                  
  ;                                  
  ;
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "next")
     (term ((local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "next" \]) (a)) == 1)))
              end)
                  
            (local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "next" \]) (a nil)) == 1)))
              end)
                  
            (local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "next" \]) (a 1)) == nil)))
              end)
                  
            (local a = (\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \}) in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "next" \]) (a 1)) == 3)))
              end))))))
  ;  
  ;                                          
  ;                     ;                    
  ;                                          
  ;                                          
  ;                                          
  ;   ;;;;;     ;;;   ;;;      ;;;;    ;;;;  
  ;   ;;  ;;   ;   ;    ;      ;;  ;  ;    ; 
  ;   ;    ;       ;    ;      ;      ;      
  ;   ;    ;   ;;;;;    ;      ;       ;;;;  
  ;   ;    ;  ;    ;    ;      ;           ; 
  ;   ;;  ;;  ;   ;;    ;      ;      ;    ; 
  ;   ;;;;;    ;;; ;  ;;;;;    ;       ;;;;  
  ;   ;                                      
  ;   ;                                      
  ;   ;                                      
                                
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "pairs" "next" "setmetatable")
     (term ((local a = (\{ (\[ "a" \] = 1) (\[ "b" \] = 2) \}) in
              (local it table index = ((_ENV \[ "pairs" \]) (a))
                in
                (($statFCall (_ENV \[ "assert" \])
                               ((it == (_ENV \[ "next" \]))))
                 ($statFCall (_ENV \[ "assert" \]) ((table == a)))
                 ($statFCall (_ENV \[ "assert" \]) ((index == nil)))
                                                                                  
                 ((_ENV \[ "next" \]) = 1)
                 (it = ((_ENV \[ "pairs" \]) (a)))
                 ; now, (_ENV \[ "next" \]) has 1 as value.
                 ($statFCall (_ENV \[ "assert" \])
                               ((not (it == (_ENV \[ "next" \])))))
                                                                                  
                 ; however, "it" points to the "next" built-in service
                 ($statFCall (_ENV \[ "assert" \]) (((it (a)) == "a")))
                 ($statFCall (_ENV \[ "assert" \]) (((it (a "a")) == "b")))
                 ($statFCall (_ENV \[ "assert" \]) (((it (a "b")) == nil)))
                                                                                  
                 ; the iterator returned in successive calls
                 ; is the same
                 (local it2 table2 index2 = ((_ENV \[ "pairs" \]) (a))
                   in
                   ($statFCall (_ENV \[ "assert" \]) ((it2 == it)))
                                                                                    
                   end)
                 )
                end)
              end)
            (local a b = (\{ \}) (\{ (\[ "__pairs" \]
                                         = (function $func1 ()
                                                     (return (< 1 2 3 >))
                                                     end)) \}) in
              (($statFCall (_ENV \[ "setmetatable" \]) (a b))
               (local it table index = ((_ENV \[ "pairs" \]) (a))
                 in
                 (($statFCall (_ENV \[ "assert" \]) ((it == 1)))
                  ($statFCall (_ENV \[ "assert" \]) ((table == 2)))
                  ($statFCall (_ENV \[ "assert" \]) ((index == 3)))
                                                                                   
                  ; The iterator returned in successive calls
                  ; is the same
                  (local it2 table2 index2 = ((_ENV \[ "pairs" \]) (a))
                    in
                                                                                     
                    ($statFCall (_ENV \[ "assert" \]) ((it2 == it)))
                                                                                     
                    end)
                  )
                 end))
              end))))))
  ;                                          
  ;                           ;;;     ;;;    
  ;                             ;       ;    
  ;                             ;       ;    
  ;                             ;       ;    
  ;   ;;;;;     ;;;     ;;;     ;       ;    
  ;   ;;  ;;   ;   ;   ;   ;    ;       ;    
  ;   ;    ;  ;            ;    ;       ;    
  ;   ;    ;  ;        ;;;;;    ;       ;    
  ;   ;    ;  ;       ;    ;    ;       ;    
  ;   ;;  ;;   ;   ;  ;   ;;    ;       ;    
  ;   ;;;;;     ;;;    ;;; ;     ;;;     ;;; 
  ;   ;                                      
  ;   ;                                      
  ;   ;
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "pcall")
     (term (local f = (function $func ()
                                ($statFCall (_ENV \[ "assert" \]) (false))
                                end)
             in ($statFCall (_ENV \[ "pcall" \]) (f))
             end)))))
                                                                          
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;    ;;;;     ;;;  ;      ;  ;;;;;   ;;;;   ;;;;;; 
  ;    ;;  ;   ;   ; ;      ; ;;  ;;  ;;  ;;    ;    
  ;    ;           ;  ; ;; ;  ;    ;  ;    ;    ;    
  ;    ;       ;;;;;  ; ;; ;  ;    ;  ;;;;;;    ;    
  ;    ;      ;    ;  ; ;; ;  ;    ;  ;         ;    
  ;    ;      ;   ;;   ;  ;   ;;  ;;  ;;   ;    ;    
  ;    ;       ;;; ;   ;  ;    ;;; ;   ;;;;      ;;; 
  ;                                ;                 
  ;                            ;   ;                 
  ;                             ;;;                  
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "rawget" "pcall")
     (term ((local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "rawget" \]) (a 1)) == 2)))
              end)
            (local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "rawget" \]) (a 3)) == nil)))
              end)
                              
            (local status errmessage = ((_ENV \[ "pcall" \])
                                        ((_ENV \[ "rawget" \]) 1 2))
              in
              (($statFCall (_ENV \[ "assert" \]) ((status == false)))
               ($statFCall
                (_ENV \[ "assert" \])
                ((errmessage == "erroneous actual parameters to rawget"
                             ))))
              end))))))
                                  
                                  
                                                                          
  ;                    
  ;                             ;                    
  ;                             ;                    
  ;                             ;                    
  ;    ;;;;     ;;;  ;      ;   ;      ;;;;   ; ;;;  
  ;    ;;  ;   ;   ; ;      ;   ;     ;;  ;;  ;;   ; 
  ;    ;           ;  ; ;; ;    ;     ;    ;  ;    ; 
  ;    ;       ;;;;;  ; ;; ;    ;     ;;;;;;  ;    ; 
  ;    ;      ;    ;  ; ;; ;    ;     ;       ;    ; 
  ;    ;      ;   ;;   ;  ;     ;     ;;   ;  ;    ; 
  ;    ;       ;;; ;   ;  ;      ;;;   ;;;;   ;    ; 
  ;                                                  
  ;                                                  
  ;
                                
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "rawlen")
     (term ((local a = (\{ (\[ 1 \] = 3) (\[ 2 \] = 4) \}) in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "rawlen" \]) (a)) == 2)))
              end)
            (local a = "añ" in
              ($statFCall (_ENV \[ "assert" \])
                            ((((_ENV \[ "rawlen" \]) (a)) == 3)))
              end))))))
            
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;    ;;;;     ;;;  ;      ;  ;;;;    ;;;;   ;;;;;; 
  ;    ;;  ;   ;   ; ;      ; ;    ;  ;;  ;;    ;    
  ;    ;           ;  ; ;; ;  ;       ;    ;    ;    
  ;    ;       ;;;;;  ; ;; ;   ;;;;   ;;;;;;    ;    
  ;    ;      ;    ;  ; ;; ;       ;  ;         ;    
  ;    ;      ;   ;;   ;  ;   ;    ;  ;;   ;    ;    
  ;    ;       ;;; ;   ;  ;    ;;;;    ;;;;      ;;; 
  ;                                                  
  ;                                                  
  ;                                                  
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "rawset" "rawget" "pcall")
     (term ((local a = (\{ \}) in
              (($statFCall (_ENV \[ "rawset" \]) (a 1 2))
               ($statFCall  (_ENV \[ "assert" \])
                              ((((_ENV \[ "rawget" \]) (a 1)) == 2))))
              end)
                              
            (local a = 1 in
              (local status errmessage = ((_ENV \[ "pcall" \])
                                          ((_ENV \[ "rawset" \]) a 1 2))
                in
                (($statFCall (_ENV \[ "assert" \]) ((status == false)))
                 ($statFCall
                  (_ENV \[ "assert" \])
                  ((errmessage == "erroneous actual parameters to rawset"
                               ))))
                end)
              end))))))
                                                              
  ;                            
  ;                     ;                            
  ;                     ;                       ;    
  ;                     ;                       ;    
  ;    ;;;;    ;;;;     ;      ;;;;     ;;;   ;;;;;; 
  ;   ;    ;  ;;  ;;    ;     ;;  ;;   ;   ;    ;    
  ;   ;       ;    ;    ;     ;    ;  ;         ;    
  ;    ;;;;   ;;;;;;    ;     ;;;;;;  ;         ;    
  ;        ;  ;         ;     ;       ;         ;    
  ;   ;    ;  ;;   ;    ;     ;;   ;   ;   ;    ;    
  ;    ;;;;    ;;;;      ;;;   ;;;;     ;;;      ;;; 
  ;                                                  
  ;                                                  
  ;                                                  
                                
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "select")
     (term (($statFCall (_ENV \[ "assert" \])
                          ((((_ENV \[ "select" \]) (1 2)) == 2)))
            ($statFCall (_ENV \[ "assert" \])
                          ((((_ENV \[ "select" \]) ("#" 1 2 3)) == 3)))
            ($statFCall (_ENV \[ "assert" \])
                          ((((_ENV \[ "select" \]) ("#")) == 0))))))))
                                  
                            
                        
  ;                                                                                                  
  ;                                                                           ;       ;;;            
  ;                                                                           ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;                     ;                       ;               ;             ;         ;            
  ;    ;;;;    ;;;;   ;;;;;;  ;;;;;;;  ;;;;   ;;;;;;    ;;;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
  ;   ;    ;  ;;  ;;    ;     ;  ;  ; ;;  ;;    ;      ;   ;    ;      ;   ;  ;;  ;;    ;     ;;  ;; 
  ;   ;       ;    ;    ;     ;  ;  ; ;    ;    ;          ;    ;          ;  ;    ;    ;     ;    ; 
  ;    ;;;;   ;;;;;;    ;     ;  ;  ; ;;;;;;    ;      ;;;;;    ;      ;;;;;  ;    ;    ;     ;;;;;; 
  ;        ;  ;         ;     ;  ;  ; ;         ;     ;    ;    ;     ;    ;  ;    ;    ;     ;      
  ;   ;    ;  ;;   ;    ;     ;  ;  ; ;;   ;    ;     ;   ;;    ;     ;   ;;  ;;  ;;    ;     ;;   ; 
  ;    ;;;;    ;;;;      ;;;  ;  ;  ;  ;;;;      ;;;   ;;; ;     ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
  ;                                                                                                  
  ;                                                                                                  
  ;                                                                                                                                                                                                            
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "setmetatable" "pcall")
     (term (local a b c = (\{ \}) (\{ (\[ "__metatable" \] = 1) \}) (\{ \}) in
             (($statFCall (_ENV \[ "setmetatable" \]) (a b))
              (local status errmessage = ((_ENV \[ "pcall" \])
                                          ((_ENV \[ "setmetatable" \]) a c))
                in
                (($statFCall (_ENV \[ "assert" \]) ((status == false)))
                 ($statFCall
                  (_ENV \[ "assert" \])
                  ((errmessage == "cannot change a protected metatable"))))
                end))
             end)))))
                        
                                                                                                       
  ;                                                  
  ;                             ;                    
  ;                                                  
  ;             ;                                    
  ;             ;                                    
  ;    ;;;;   ;;;;;;   ;;;;   ;;;     ; ;;;    ;;;;; 
  ;   ;    ;    ;      ;;  ;    ;     ;;   ;  ;;  ;; 
  ;   ;         ;      ;        ;     ;    ;  ;    ; 
  ;    ;;;;     ;      ;        ;     ;    ;  ;    ; 
  ;        ;    ;      ;        ;     ;    ;  ;    ; 
  ;   ;    ;    ;      ;        ;     ;    ;  ;;  ;; 
  ;    ;;;;      ;;;   ;      ;;;;;   ;    ;   ;;; ; 
  ;                                                ; 
  ;                                            ;   ; 
  ;                                             ;;;  
                                  
                                  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;    ;;;;    ;;;;   ;;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;; 
  ;    ;      ;    ;  ;    ; 
  ;    ;      ;;;;;;  ;    ; 
  ;    ;      ;       ;    ; 
  ;    ;      ;;   ;  ;;  ;; 
  ;    ;       ;;;;   ;;;;;  
  ;                   ;      
  ;                   ;      
  ;                   ;      
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "string" "string.rep")
     (term (($statFCall (_ENV \[ "assert" \])
                          (((((_ENV \[ "string" \]) \[ "rep" \]) ("a" 4))
                            == "aaaa")))
            ($statFCall (_ENV \[ "assert" \])
                          (((((_ENV \[ "string" \]) \[ "rep" \]) ("a" 4 "b"))
                            == "abababa"))))))))
                                  
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
                                  
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "table" "table.pack")
     (term (local table = (((_ENV \[ "table" \]) \[ "pack" \]) ("a" "b" "c")) in
             (($statFCall (_ENV \[ "assert" \]) (((table \[ 1 \]) == "a")))
              ($statFCall (_ENV \[ "assert" \]) (((table \[ 2 \]) == "b")))
              ($statFCall (_ENV \[ "assert" \]) (((table \[ 3 \]) == "c")))
              ($statFCall (_ENV \[ "assert" \]) (((table \[ "n" \]) == 3))))
             end)))))
                                  
                                  
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
                                  
  (test-predicate
   (redex-match ext-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (plugIntoExecutionEnvironment
     services
     '("assert" "table" "table.unpack")
     (term (local table = (\{ "a" "b" "c" \}) in
             (local v1 v2 v3 = (((_ENV \[ "table" \]) \[ "unpack" \]) (table))
               in
               (($statFCall (_ENV \[ "assert" \]) ((v1 == "a")))
                ($statFCall (_ENV \[ "assert" \]) ((v2 == "b")))
                ($statFCall (_ENV \[ "assert" \]) ((v3 == "c"))))
               end)
             end)))))
  (test-results))

(provide standard-library-test-suite)