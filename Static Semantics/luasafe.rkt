#lang racket

(require  redex
          "./typing.rkt"
          "./type_inference.rkt"
          "./reaching_defs.rkt"
          "../Desugar/parser.rkt")


;                                                                 
;                                                                 
;                                                                 
;  ;;;;                          ;;;;                ;;;          
;     ;                        ;;   ;;              ;             
;     ;                        ;                    ;             
;     ;      ;     ;    ;;;;   ;          ;;;;    ;;;;;;    ;;;   
;     ;      ;     ;   ;    ;  ;;        ;    ;     ;      ;   ;  
;     ;      ;     ;        ;    ;;;          ;     ;     ;     ; 
;     ;      ;     ;   ;;;;;;       ;;   ;;;;;;     ;     ;     ; 
;     ;      ;     ;  ;;    ;        ;  ;;    ;     ;     ;;;;;;; 
;     ;      ;     ;  ;     ;        ;  ;     ;     ;     ;       
;     ;      ;;   ;;  ;    ;;  ;;   ;;  ;    ;;     ;      ;    ; 
;      ;;;    ;;;; ;   ;;;; ;   ;;;;;    ;;;; ;     ;       ;;;;  
;                                                                 
;                                                                 
;                                                                 
;                                                                 

(define (luasafe program)
  (begin
    ; infer types of program
    (println "type inference: ")
    
    (define typed_program
      (term (type_term
             (in-hole 
              (local $ENV = (\{ (\[ "setmetatable" \] =
                                    (function $1 (table meta)
                                              \;
                                              end)) \})
                in hole end)
              ,(parse-this program #f (void))))))
    (println "  done")
    
    (if (equal? (term (-> "the term cannot be typed" <-)) typed_program)

        typed_program

        (judgment-holds
         (type_s · · (reach_defs ,typed_program) hole
                 (unquote typed_program)
                 Γ))
     )
    )
  )

(provide luasafe)