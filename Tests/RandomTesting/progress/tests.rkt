#lang racket

(require redex
         "../../../grammar.rkt"
         "./defs.rkt")


;                                                                                                     
;                                                                                                     
;                                                                                                     
;                    ;;;;     ;;;;                   ;;;                                            ; 
;                       ;        ;                  ;                                               ; 
;                       ;        ;                  ;                                               ; 
;  ;       ;   ;;;      ;        ;                ;;;;;;    ;;;      ; ;;;  ;;;;;;     ;;;      ;;; ; 
;  ;       ;  ;   ;     ;        ;                  ;      ;   ;     ;;   ; ;  ;  ;   ;   ;    ;   ;; 
;   ;  ;  ;  ;     ;    ;        ;                  ;     ;     ;    ;      ;  ;  ;  ;     ;  ;     ; 
;   ;  ;  ;  ;     ;    ;        ;        ;;;;      ;     ;     ;    ;      ;  ;  ;  ;     ;  ;     ; 
;   ; ; ; ;  ;;;;;;;    ;        ;                  ;     ;     ;    ;      ;  ;  ;  ;;;;;;;  ;     ; 
;   ; ; ; ;  ;          ;        ;                  ;     ;     ;    ;      ;  ;  ;  ;        ;     ; 
;    ;   ;    ;    ;    ;        ;                  ;      ;   ;     ;      ;  ;  ;   ;    ;   ;   ;; 
;    ;   ;     ;;;;      ;;;      ;;;               ;       ;;;      ;      ;  ;  ;    ;;;;     ;;; ; 
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     

(define (well-formed-test-suite)

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
  ; skip stat
  (test-equal
   (judgment-holds
    (well_formed_term hole () () \;))
   #t)

  ; Break
  (test-equal
   (judgment-holds
    (well_formed_term hole () () break))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term (while true do hole end) () () break))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term (do (hole Break) end) () () break))
   #t)

  ; return
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (return 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (return 1 2 3)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (return 1 (function x (y) break end))))
   #f)

  ; fun call
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (1 ())))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (1 (2 3))))
   #t)
  
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (1 ((function x (y) break end)))))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (1 (2 (function x (y) break end)))))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (1 : x ((function x (y) break end)))))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (1 : x ())))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (1 : x (2 3))))
   #t)

  ; assignment
  ; var not defined
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (x = 1)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (local x y = 1 2 in (x y = 3 4) end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (local x y = 1 2 in (x y = 3 z) end)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole (((ref 1) 1)) () ((ref 1) = 2)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () (((objr 1) ((\{ \}) nil 0)))
                      (((objr 1) \[ 1 \]) = 2)))
   #t)

  ; do - end
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (do (function x (y) break end) end)))
   #f)

  ; conditional
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (if 1 then \; else \; end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (if (function x (y) break end)
                                     then \; else \; end)))
   #f)

  ; while
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (while 1 do \; end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (while (function x (y) break end)
                                        do \; end)))
   #f)

  ; local var
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (local x = 1 in \; end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (local x = 1 in (x = 1) end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (local x = 1 in (y = 1) end)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (local x y = 1 2 in (y = 1) end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (local x y = 1 z in \; end)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (\; ((rEnv (ref 1))) LocalBody)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole (((ref 1) 1)) () (\; ((rEnv (ref 1))) LocalBody)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole (((ref 1) 1)) () (break ((rEnv (ref 1))) LocalBody)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole (((ref 1) 1)) ()
                      (\; ((rEnv (ref 1)) (rEnv (ref 2))) LocalBody)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole (((ref 1) 1) ((ref 2) 1)) ()
                      (\; ((rEnv (ref 1)) (rEnv (ref 2))) LocalBody)))
   #t)
  
  ; concat
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (\; break)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (break \;)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (\; \;)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (\; \; \;)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (\; \; break)))
   #f)

  ; error obj
  (test-equal
   (judgment-holds
    (well_formed_term hole () () ($err 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () ($err (function x (y) break end))))
   #f)

  
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

  ; Break tag
  (test-equal
   (judgment-holds
    (well_formed_term hole () () (($iter true do \; end) Break)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () ()
                      ((if true then
                           (\; ($iter true do \; end))
                           else \; end) Break)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () ()
                      ((\; ($iter true do \; end)) Break)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () (\; Break)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () ((if true then \; else \; end) Break)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole () ()
                      ((\; ($iter x do \; end)) Break)))
   #f)

  ; $iter
  
  (test-equal
   (judgment-holds
    (well_formed_term (hole Break) () () ($iter true do \; end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole () () ($iter true do \; end)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term (hole Break) () () ($iter true do (x = 1)
                                                end)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term (hole Break) () () ($iter x do \;
                                                end)))
   #f)

  ; WrongKey
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 0)))
                      ((((objr 1) \[ 1 \]) = 2) WrongKey)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 0)))
                      ((((objr 1) \[ (function x (y) break end) \]) = 2)
                       WrongKey)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 0)))
                      ((((objr 1) \[ (function x (y) break end) \]) = 2)
                       WrongKey)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 0)))
                      ((((objr 1) \[ 1 \]) = (function x (y) break end))
                       WrongKey)))
   #f)

  ; NonTable
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 0)))
                      (((1 \[ 2 \]) = 3)
                       NonTable)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((((objr 1) \[ 2 \]) = 3)
                       NonTable)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (((1 \[ (function x (y) break end) \]) = 3)
                       NonTable)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (((1 \[ 2 \]) = (function x (y) break end))
                       NonTable)))
   #f)

  ; WrongFunCall
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (($statFunCall (function x (y) break end) ())
                       WrongFunCall)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (($statFunCall 1 (1)) WrongFunCall)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 (1 2 3)) WrongFunCall)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 ((function x (y) break end))) WrongFunCall)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 (2 (function x (y) break end))) WrongFunCall)))
   #f)

  ; FunCall
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\; () RetStat)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (break () RetStat)))
   #f)
  
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

  ; primitive types
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      nil))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      true))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      false))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      1))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      "asd"))
   #t)

  ; objref
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 0)))
                      (objr 1)))
   #t)

  ; functiondef
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (function x (y) break end)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (function x (y) \; end)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term (function x (<<<) hole end)
                      ()
                      ()
                      <<<))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      <<<))
   #f)

  ; Name
  (test-equal
   (judgment-holds
    (well_formed_term (local x = 1 in hole end)
                      ()
                      ()
                      x))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term (function x (y) hole end)
                      ()
                      ()
                      y))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term (local x = 1 in hole end)
                      ()
                      ()
                      y))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term (function x (y) hole end)
                      ()
                      ()
                      z))
   #f)

  ; table field
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (1 \[ 2 \])))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (x \[ 2 \])))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (1 \[ x \])))
   #f)

  ; parenthesized exp
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\( 1 \))))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\( x \))))
   #f)

  ; built-in
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ($builtIn print (1))))
   #t)
  
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ($builtIn print (x))))
   #f)

  ; tableconstructor
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\{ (\[ 1 \] = 2) \})))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\{ (\[ 1 \] = 2) (\[ 3 \] = 4)  \})))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\{ (\[ x \] = 2) \})))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\{ (\[ 1 \] = 2) (\[ x \] = 3) \})))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (\{ (\[ 1 \] = x) \})))
   #f)

  ; binops
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (1 + 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (x + 1)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (1 + x)))
   #f)

  ; unops
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (- 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (- x)))
   #f)

  ; val refs
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      (((ref 1) 1))
                      ()
                      (ref 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (ref 1)))
   #f)

  ; tuples
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (< 1 >)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (< 1 2 3 >)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (< x >)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (< 1 x >)))
   #f)

  
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

  ; protected mode
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((\; () RetExp) ProtectedMode)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((< 1 >) ProtectedMode)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (((1 (2)) WrongFunCall) ProtectedMode)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 (2)) ProtectedMode)))
   #t)

  ; xpcall
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((\; () RetExp) ProtectedMode 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((< 1 >) ProtectedMode 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (((1 (2)) WrongFunCall) ProtectedMode 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 (2)) ProtectedMode 1)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 (2)) ProtectedMode x)))
   #f)

  ; NonTable
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 \[ 2 \]) NonTable)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((x \[ 2 \]) NonTable)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 \[ x \]) NonTable)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (((objr 1) \[ x \]) NonTable)))
   #f)

  ; WrongKey
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 1)))
                      (((objr 1) \[ 1 \]) WrongKey)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((x \[ 1 \]) WrongKey)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      (((objr 1) ((\{ \}) nil 1)))
                      (((objr 1) \[ x \]) WrongKey)))
   #f)

  ; ArithWrongOps
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 + true) ArithWrongOps)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((x + true) ArithWrongOps)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 + x) ArithWrongOps)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 + 1) ArithWrongOps)))
   #f)

  ; StrConcatWrongOps
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      (("a" .. "b") StrConcatWrongOps)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((x .. 1) StrConcatWrongOps)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 .. x) StrConcatWrongOps)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((true .. "b") StrConcatWrongOps)))
   #t)

  ; OrdCompWrongOps
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((true < 1) OrdCompWrongOps)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((x < 1) OrdCompWrongOps)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((true < x) OrdCompWrongOps)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 < 1) OrdCompWrongOps)))
   #f)

  ; StrLenWrongOp
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((\# 1)StrLenWrongOp)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((\# "a")StrLenWrongOp)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((\# x)StrLenWrongOp)))
   #f)

  ; NegWrongOp
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((- true)NegWrongOp)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((- 1)NegWrongOp)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((- x)NegWrongOp)))
   #f)

  ; EqFail
  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 == 2) EqFail)))
   #t)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 == 1) EqFail)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((x == 2) EqFail)))
   #f)

  (test-equal
   (judgment-holds
    (well_formed_term hole
                      ()
                      ()
                      ((1 == x) EqFail)))
   #f)
  
  (test-results)
  )

(provide well-formed-test-suite)


;                                                                                   
;                                                                                   
;                                                                                   
;       ;;;                                                            ;;;          
;      ;                                                              ;             
;      ;                                                              ;             
;    ;;;;;;    ; ;;;    ;;;      ;;;               ; ;;;    ;;;     ;;;;;;   ;;;;;  
;      ;       ;;   ;  ;   ;    ;   ;              ;;   ;  ;   ;      ;     ;     ; 
;      ;       ;      ;     ;  ;     ;             ;      ;     ;     ;     ;       
;      ;       ;      ;     ;  ;     ;             ;      ;     ;     ;     ;;;;    
;      ;       ;      ;;;;;;;  ;;;;;;;             ;      ;;;;;;;     ;         ;;; 
;      ;       ;      ;        ;                   ;      ;           ;           ; 
;      ;       ;       ;    ;   ;    ;             ;       ;    ;     ;     ;     ; 
;      ;       ;        ;;;;     ;;;;              ;        ;;;;      ;      ;;;;;  
;                                                                                   
;                                                                                   
;                                                                                   
;                                                                                   

(define (free-refs-test-suite)
  
  ;           
  ;           
  ;           
  ;           
  ;           
  ;           
  ;     ; ;;; 
  ;     ;;   ;
  ;     ;     
  ;     ;     
  ;     ;     
  ;     ;     
  ;     ;     
  ;     ;     
  ;           
  ;           
  ;           
  ;           

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
  ; skip stat
  (test-equal
   (term (free_val_ref () \;))
   '())

  (test-equal
   (term (free_val_ref () break))
   '())

  ; while
  (test-equal
   (term (free_val_ref () (while (ref 1) do \; end)))
   (term ((ref 1))))

  (test-equal
   (term (free_val_ref (((ref 1) 1)) (while (ref 1) do \; end)))
   '())

  ; do-end
  (test-equal
   (term (free_val_ref () (do \; end)))
   '())

  (test-equal
   (term (free_val_ref () (do (while (ref 1) do \; end) end)))
   (term ((ref 1))))

  ; return
  (test-equal
   (term (free_val_ref () (return 1 (ref 1))))
   (term ((ref 1))))

  (test-equal
   (term (free_val_ref (((ref 1) false)) (return 1 (ref 1))))
   '())
  
  ; fun call
  (test-equal
   (term (free_val_ref () ($statFunCall (ref 1) ())))
   (term ((ref 1))))

  (test-equal
   (term (free_val_ref (((ref 1) false)) ($statFunCall (ref 1) ())))
   '())

  (test-equal
   (term (free_val_ref () ($statFunCall (ref 1) ((ref 2)))))
   (term ((ref 1) (ref 2))))

  (test-equal
   (term (free_val_ref (((ref 1) false)) ($statFunCall (ref 1) ((ref 2)))))
   (term ((ref 2))))

  (test-equal
   (term (free_val_ref () ($statFunCall (ref 1) : x ())))
   (term ((ref 1))))

  (test-equal
   (term (free_val_ref (((ref 1) false)) ($statFunCall (ref 1) : x ())))
   '())

  (test-equal
   (term (free_val_ref (((ref 1) false)) ($statFunCall (ref 1) : x ((ref 2)))))
   (term ((ref 2))))

  ; assignment
  (test-equal
   (term (free_val_ref (((ref 1) false)) ((ref 1) = 1)))
   '())

  (test-equal
   (term (free_val_ref () ((ref 1) = 1)))
   (term ((ref 1))))

  (test-equal
   (term (free_val_ref () ((ref 1) = (ref 2))))
   (term ((ref 1) (ref 2))))

  (test-equal
   (term (free_val_ref (((ref 1) false)) ((ref 1) = (ref 2))))
   (term ((ref 2))))

  (test-equal
   (term (free_val_ref () (local x y = (ref 1) 2 in \; end)))
   (term ((ref 1))))
  
  (test-equal
   (term (free_val_ref (((ref 1) false)) (local x y = (ref 1) 2 in
                                           (x y = 3 (ref 2))
                                           end)))
   (term ((ref 2))))
  
  (test-equal
   (term (free_val_ref (((ref 1) (objr 1))) (((ref 1) \[ 1 \]) = 2)))
   '())

  (test-equal
   (term (free_val_ref () (((ref 1) \[ 1 \]) = 2)))
   (term ((ref 1))))

  ; conditional
  (test-equal
   (term (free_val_ref () (if (ref 1)
                              then ((ref 2) = 1)
                              else ((ref 3) = 1) end)))
   (term ((ref 1) (ref 2) (ref 3))))

  (test-equal
   (term (free_val_ref (((ref 1) true)) (if (ref 1)
                                            then ((ref 2) = 1)
                                            else ((ref 3) = 1) end)))
   (term ((ref 2) (ref 3))))

  ; local
  (test-equal
   (term (free_val_ref () (\; ((rEnv (ref 1)) (rEnv (ref 2))) LocalBody)))
   (term ((ref 2) (ref 1))))

  (test-equal
   (term (free_val_ref (((ref 1) 1)) (\; ((rEnv (ref 1))) LocalBody)))
   '())

  (test-equal
   (term (free_val_ref (((ref 1) 1)) (break ((rEnv (ref 1))) LocalBody)))
   '())
  
  ; concat
  (test-equal
   (term (free_val_ref (((ref 1) 1)) (((ref 1) = (ref 2)) \;)))
   (term ((ref 2))))
  
  (test-equal
     (term (free_val_ref (((ref 1) 1)) (\; ((ref 1) = (ref 2)))))
     (term ((ref 2))))
  
    ; error obj
    (test-equal
     ; error obj cannot have a ref...
     (term (free_val_ref () ($err 1)))
     '())
  
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
  
    ; Break tag
  (test-equal
   (term (free_val_ref (((ref 1) 1)) (($iter (ref 1) do \; end) Break)))
   '())
  
  (test-equal
   (term (free_val_ref () (($iter (ref 1) do \; end) Break)))
   (term ((ref 1))))
  
  ; $iter
  (test-equal
   (term (free_val_ref () ($iter (ref 1) do \; end)))
   (term ((ref 1))))

  (test-equal
   (term (free_val_ref (((ref 1) 1)) ($iter (ref 1) do \; end)))
   '())
  
  ; WrongKey
  (test-equal
   ; just testing the syntactic form; WrongKey forces values and tids as tables
   (term (free_val_ref () ((((objr 1) \[ 1 \]) = 2) WrongKey)))
   '())
  
  ; NonTable
  (test-equal
   ; only values
   (term (free_val_ref () (((1 \[ 2 \]) = 3) NonTable)))
   '())
  
  ; WrongFunCall
  (test-equal
   ; only values
   (term (free_val_ref () (($statFunCall 1 (2)) WrongFunCall)))
   '())
  
  ; FunCall
  (test-equal
   (term (free_val_ref (((ref 1) 1))
                       (($statFunCall (ref 1) ((ref 2))) () RetStat)))
   (term ((ref 2))))
    
                                   
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
  
    ; primitive types
    (test-equal
     (term (free_val_ref () nil))
     '())
  
    (test-equal
     (term (free_val_ref () true))
     '())
  
    (test-equal
     (term (free_val_ref () 1))
     '())
  
    (test-equal
     (term (free_val_ref () "asd"))
     '())
  
    ; objref
    (test-equal
     (term (free_val_ref () (objr 1)))
     '())
  
    ; functiondef
    (test-equal
     (term (free_val_ref () (function x (y) ((ref 1) = 1) end)))
     (term ((ref 1))))

    (test-equal
     (term (free_val_ref (((ref 1 ) 1)) (function x (y) ((ref 1) = 1) end)))
     '())

  (test-equal
     (term (free_val_ref () (function x (<<<) ((ref 1) = 1) end)))
     (term ((ref 1))))

    (test-equal
     (term (free_val_ref (((ref 1 ) 1)) (function x (<<<) ((ref 1) = 1) end)))
     '())

  ; Name
    (test-equal
     (term (free_val_ref () x))
     '())
  
    ; table field
    (test-equal
     (term (free_val_ref () ((ref 1) \[ (ref 2) \])))
     (term ((ref 1) (ref 2))))

  (test-equal
     (term (free_val_ref (((ref 2) 1)) ((ref 1) \[ (ref 2) \])))
     (term ((ref 1))))
  
    ; parenthesized exp
    (test-equal
     (term (free_val_ref () (\( (ref 1) \))))
     (term ((ref 1))))

    (test-equal
     (term (free_val_ref (((ref 1) 1)) (\( (ref 1) \))))
     '())
  
    ; built-in
    (test-equal
     (term (free_val_ref (((ref 1) 1)) ($builtIn print ((ref 1)))))
     '())
    
    (test-equal
     (term (free_val_ref () ($builtIn print ((ref 1)))))
     (term ((ref 1))))
  
    ; tableconstructor
    (test-equal
     (term (free_val_ref () (\{ (\[ (ref 1) \] = (ref 2)) \})))
     (term ((ref 1) (ref 2))))
  
    (test-equal
     (term (free_val_ref (((ref 1) 1))
                         (\{ (\[ (ref 1) \] = (ref 2)) \})))
     (term ((ref 2))))
  
    ; binops
    (test-equal
     (term (free_val_ref (((ref 1) 1)) ((ref 1) + (ref 2))))
     (term ((ref 2))))
  
    ; unops
    (test-equal
     (term (free_val_ref (((ref 1) 1)) (- (ref 1))))
     '())
  
    (test-equal
     (term (free_val_ref () (- (ref 1))))
     (term ((ref 1))))
  
    ; val refs
    (test-equal
     (term (free_val_ref (((ref 1) 1)) (ref 1)))
     '())

  (test-equal
   (term (free_val_ref () (ref 1)))
   (term ((ref 1))))
  
  
    ; tuples
    (test-equal
     (term (free_val_ref (((ref 1) 1)) (< (ref 1) >)))
     '())
  
    (test-equal
     (term (free_val_ref () (< (ref 1) >)))
     (term ((ref 1))))
  
    
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
  
    ; protected mode
    (test-equal
     (term (free_val_ref () ((((ref 1) = 1) () RetExp) ProtectedMode)))
     (term ((ref 1))))

  (test-equal
     (term (free_val_ref (((ref 1) 1))
                         ((((ref 1) = 1) () RetExp) ProtectedMode)))
     '())
  
    ; xpcall
  (test-equal
     (term (free_val_ref () ((((ref 1) = 1) () RetExp) ProtectedMode 1)))
     (term ((ref 1))))

  (test-equal
     (term (free_val_ref (((ref 1) 1))
                         ((((ref 1) = 1) () RetExp) ProtectedMode 1)))
     '())
  
  
    ; NonTable
    (test-equal
     ;only values
     (term (free_val_ref () ((1 \[ 2 \]) NonTable)))
     '())
  
    ; WrongKey
    (test-equal
     (term (free_val_ref () (((objr 1) \[ 1 \]) WrongKey)))
     '())
  
    ; ArithWrongOps
    (test-equal
     (term (free_val_ref () ((1 + true) ArithWrongOps)))
     '())
  
    ; StrConcatWrongOps
    (test-equal
     (term (free_val_ref () (("a" .. "b") StrConcatWrongOps)))
     '())
  
  
    ; OrdCompWrongOps
    (test-equal
     (term (free_val_ref () ((true < 1) OrdCompWrongOps)))
     '())
  
    ; StrLenWrongOp
    (test-equal
     (term (free_val_ref () ((\# 1)StrLenWrongOp)))
     '())
  
    ; NegWrongOp
    (test-equal
     (term (free_val_ref () ((- true)NegWrongOp)))
     '())
  
    ; EqFail
    (test-equal
     (term (free_val_ref () ((1 == 2) EqFail)))
     '())
  
  (test-results)
  )

(provide free-refs-test-suite)