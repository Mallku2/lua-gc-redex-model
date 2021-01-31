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
             (term ($statFunCall ($ENV \[ "type" \]) ())))
            
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
             (term ($statFunCall ($ENV \[ "assert" \]) ())))
            
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
     (term ((local status errmessage = (($ENV \[ "pcall" \]) (($ENV \[ "assert" \]) false "error message"))
              in
              (($statFunCall ($ENV \[ "assert" \]) ((status == false)))
               ($statFunCall ($ENV \[ "assert" \])
                             ((errmessage == "error message"))))
              end)
             
            ($statFunCall ($ENV \[ "assert" \]) (true "error message"))
            ($statFunCall ($ENV \[ "assert" \]) (1 2 3 4 5)))))))
          
          
          
            
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
     (term (local status errmessage = (($ENV \[ "pcall" \])
                                        (($ENV \[ "error" \])
                                         "error message"))
              in
              (($statFunCall ($ENV \[ "assert" \]) ((status == false)))
               ($statFunCall ($ENV \[ "assert" \])
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
             (($statFunCall ($ENV \[ "setmetatable" \]) (a b))
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "getmetatable" \]) (a)) == b))))
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
              (local it table index = (($ENV \[ "ipairs" \]) (a))
                in
                (($statFunCall ($ENV \[ "assert" \]) ((table == a)))
                 ($statFunCall ($ENV \[ "assert" \]) ((index == 0)))
                 (local nextIndex value = (it (a 0))
                   in
                   (($statFunCall ($ENV \[ "assert" \]) ((nextIndex == 1)))
                                                             
                    ($statFunCall ($ENV \[ "assert" \]) ((value == "a")))
                                                             
                    (nextIndex value = (it (a 1)))
                                                             
                    ($statFunCall ($ENV \[ "assert" \]) ((nextIndex == 2)))
                                                             
                    ($statFunCall ($ENV \[ "assert" \]) ((value == "b")))
                                                             
                    ; The iterator returned in successive calls
                    ; is the same
                    (local it2 table2 index2 = (($ENV \[ "ipairs" \]) (a))
                      in
                                                               
                      ($statFunCall ($ENV \[ "assert" \]) ((it2 == it)))
                                                               
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
              (($statFunCall ($ENV \[ "setmetatable" \]) (a b))
               (local it table index = (($ENV \[ "ipairs" \]) (a))
                 in
                 (($statFunCall ($ENV \[ "assert" \]) ((it == 1)))
                  ($statFunCall ($ENV \[ "assert" \]) ((table == 2)))
                  ($statFunCall ($ENV \[ "assert" \]) ((index == 3)))
                                                           
                  ; The iterator returned in successive calls
                  ; is the same
                  (local it2 table2 index2 = (($ENV \[ "ipairs" \]) (a))
                    in
                                                             
                    ($statFunCall ($ENV \[ "assert" \]) ((it2 == it)))
                                                             
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
     (term ((($ENV |[| "fat" |]|)
             =
             (function
              $2
              (x)
              (if (x <= 1) then (return (< 1 >))
                  else
                  (return (x * ((($ENV |[| "load" |]|)
                                 (("return fat(" .. ((x - 1) .. ")")))) ())))
                  end)
              end))
                   
            ($statFunCall ($ENV \[ "assert" \]) ((($ENV \[ "load" \])
                                                  ("assert(fat(6)==720)"))))
            
            (local a = true in
              (local b = (($ENV |[| "load" |]|)
                          ((function $1 ()
                                     (if a then
                                         ((a = false)
                                          (return "return true"))
                                         else
                                         (return nil)
                                         end)
                                     end)))
                in
                ($statFunCall ($ENV \[ "assert" \]) ((b ())))
                end)
              end)
            
            (local proc = nil in
              ((proc = (($ENV |[| "load" |]|)
                        ("assert(type == nil); return t(1)"
                         nil
                         nil
                         (\{ (\[ "t" \] = ($ENV |[| "type" |]|))
                             (\[ "assert" \] = ($ENV |[| "assert" |]|))\}))))
               ($statFunCall ($ENV \[ "assert" \]) (((proc ()) == "number")))
            
               ($statFunCall ($ENV \[ "assert" \])
                             (((($ENV \[ "type" \]) (1)) == "number"))))
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
     (term ((($ENV \[ "fat" \]) = (($ENV \[ "loadfile" \])
                                   ("loadfile_test.lua")))
            ($statFunCall ($ENV \[ "assert" \])
                          (((($ENV \[ "fat" \]) (1)) == 1)))
            ($statFunCall ($ENV \[ "assert" \])
                          (((($ENV \[ "fat" \]) (6)) == 720))))))))
            
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
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "next" \]) (a)) == 1)))
              end)
                  
            (local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "next" \]) (a nil)) == 1)))
              end)
                  
            (local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "next" \]) (a 1)) == nil)))
              end)
                  
            (local a = (\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \}) in
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "next" \]) (a 1)) == 3)))
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
              (local it table index = (($ENV \[ "pairs" \]) (a))
                in
                (($statFunCall ($ENV \[ "assert" \])
                               ((it == ($ENV \[ "next" \]))))
                 ($statFunCall ($ENV \[ "assert" \]) ((table == a)))
                 ($statFunCall ($ENV \[ "assert" \]) ((index == nil)))
                                                                                  
                 (($ENV \[ "next" \]) = 1)
                 (it = (($ENV \[ "pairs" \]) (a)))
                 ; now, ($ENV \[ "next" \]) has 1 as value.
                 ($statFunCall ($ENV \[ "assert" \])
                               ((not (it == ($ENV \[ "next" \])))))
                                                                                  
                 ; however, "it" points to the "next" built-in service
                 ($statFunCall ($ENV \[ "assert" \]) (((it (a)) == "a")))
                 ($statFunCall ($ENV \[ "assert" \]) (((it (a "a")) == "b")))
                 ($statFunCall ($ENV \[ "assert" \]) (((it (a "b")) == nil)))
                                                                                  
                 ; the iterator returned in successive calls
                 ; is the same
                 (local it2 table2 index2 = (($ENV \[ "pairs" \]) (a))
                   in
                   ($statFunCall ($ENV \[ "assert" \]) ((it2 == it)))
                                                                                    
                   end)
                 )
                end)
              end)
            (local a b = (\{ \}) (\{ (\[ "__pairs" \]
                                         = (function $func1 ()
                                                     (return (< 1 2 3 >))
                                                     end)) \}) in
              (($statFunCall ($ENV \[ "setmetatable" \]) (a b))
               (local it table index = (($ENV \[ "pairs" \]) (a))
                 in
                 (($statFunCall ($ENV \[ "assert" \]) ((it == 1)))
                  ($statFunCall ($ENV \[ "assert" \]) ((table == 2)))
                  ($statFunCall ($ENV \[ "assert" \]) ((index == 3)))
                                                                                   
                  ; The iterator returned in successive calls
                  ; is the same
                  (local it2 table2 index2 = (($ENV \[ "pairs" \]) (a))
                    in
                                                                                     
                    ($statFunCall ($ENV \[ "assert" \]) ((it2 == it)))
                                                                                     
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
                                ($statFunCall ($ENV \[ "assert" \]) (false))
                                end)
             in ($statFunCall ($ENV \[ "pcall" \]) (f))
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
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "rawget" \]) (a 1)) == 2)))
              end)
            (local a = (\{ (\[ 1 \] = 2) \}) in
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "rawget" \]) (a 3)) == nil)))
              end)
                              
            (local status errmessage = (($ENV \[ "pcall" \])
                                        (($ENV \[ "rawget" \]) 1 2))
              in
              (($statFunCall ($ENV \[ "assert" \]) ((status == false)))
               ($statFunCall
                ($ENV \[ "assert" \])
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
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "rawlen" \]) (a)) == 2)))
              end)
            (local a = "añ" in
              ($statFunCall ($ENV \[ "assert" \])
                            (((($ENV \[ "rawlen" \]) (a)) == 3)))
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
              (($statFunCall ($ENV \[ "rawset" \]) (a 1 2))
               ($statFunCall  ($ENV \[ "assert" \])
                              (((($ENV \[ "rawget" \]) (a 1)) == 2))))
              end)
                              
            (local a = 1 in
              (local status errmessage = (($ENV \[ "pcall" \])
                                          (($ENV \[ "rawset" \]) a 1 2))
                in
                (($statFunCall ($ENV \[ "assert" \]) ((status == false)))
                 ($statFunCall
                  ($ENV \[ "assert" \])
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
     (term (($statFunCall ($ENV \[ "assert" \])
                          (((($ENV \[ "select" \]) (1 2)) == 2)))
            ($statFunCall ($ENV \[ "assert" \])
                          (((($ENV \[ "select" \]) ("#" 1 2 3)) == 3)))
            ($statFunCall ($ENV \[ "assert" \])
                          (((($ENV \[ "select" \]) ("#")) == 0))))))))
                                  
                            
                        
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
             (($statFunCall ($ENV \[ "setmetatable" \]) (a b))
              (local status errmessage = (($ENV \[ "pcall" \])
                                          (($ENV \[ "setmetatable" \]) a c))
                in
                (($statFunCall ($ENV \[ "assert" \]) ((status == false)))
                 ($statFunCall
                  ($ENV \[ "assert" \])
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
     (term (($statFunCall ($ENV \[ "assert" \])
                          ((((($ENV \[ "string" \]) \[ "rep" \]) ("a" 4))
                            == "aaaa")))
            ($statFunCall ($ENV \[ "assert" \])
                          ((((($ENV \[ "string" \]) \[ "rep" \]) ("a" 4 "b"))
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
     (term (local table = ((($ENV \[ "table" \]) \[ "pack" \]) ("a" "b" "c")) in
             (($statFunCall ($ENV \[ "assert" \]) (((table \[ 1 \]) == "a")))
              ($statFunCall ($ENV \[ "assert" \]) (((table \[ 2 \]) == "b")))
              ($statFunCall ($ENV \[ "assert" \]) (((table \[ 3 \]) == "c")))
              ($statFunCall ($ENV \[ "assert" \]) (((table \[ "n" \]) == 3))))
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
             (local v1 v2 v3 = ((($ENV \[ "table" \]) \[ "unpack" \]) (table))
               in
               (($statFunCall ($ENV \[ "assert" \]) ((v1 == "a")))
                ($statFunCall ($ENV \[ "assert" \]) ((v2 == "b")))
                ($statFunCall ($ENV \[ "assert" \]) ((v3 == "c"))))
               end)
             end)))))
  (test-results))

(provide standard-library-test-suite)