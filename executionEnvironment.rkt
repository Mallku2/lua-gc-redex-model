#lang racket
(require redex
         "./grammar.rkt"
         "./Relations/fullProgs.rkt")
; dictionary as a list of pairs of the form (service's name, code), for easy
; construction of a suitable execution environment.
(define services
  (list
   ; basic functions
   
   ; the wrappers are just vararg functions, in order to correctly model the
   ; behavior of a service call without the expected amount of actual parameters:
   ; only δ specifies the expected actual parameters, resorting to error in case
   ; of non-compliance with them
   (cons "assert"
         (term (($ENV \[ "assert" \]) = (function $assert (<<<)
                                                  (return ($builtIn assert (<<<)))
                                                  end))
               ))

   (cons "collectgarbage"
         (term (($ENV \[ "collectgarbage" \]) = (function $collectgarbage (<<<)
                                                          (return ($builtIn collectgarbage (<<<)))
                                                          end))
               ))

   (cons "error"
         (term (($ENV \[ "error" \]) = (function $error (<<<)
                                                 (return ($builtIn error (<<<)))
                                                 end))
               ))

   (cons "getmetatable"
         (term (($ENV \[ "getmetatable" \]) = (function $getmetatable (<<<)
                                                        (return
                                                         ($builtIn getmetatable
                                                                   (<<<)))
                                                        end))
               ))

   (cons "load"
         (term (($ENV \[ "load" \]) = (function $load (<<<)
                                                (return ($builtIn load (<<<)))
                                                end))
               ))

   (cons "loadfile"
         (term (($ENV \[ "loadfile" \]) = (function $loadfile (<<<)
                                                    (return ($builtIn loadfile (<<<)))
                                                    end))
               ))

   (cons "ipairs"
         (term (($ENV \[ "ipairs" \]) = (function $ipairs (<<<)
                                                  (return ($builtIn ipairs (<<<)))
                                                  end))
               ))

   (cons "next"
         (term (($ENV \[ "next" \]) = (function $next (<<<)
                                                (return ($builtIn next (<<<)))
                                                end))
               ))

   (cons "pairs"
         (term (($ENV \[ "pairs" \]) = (function $pairs (<<<)
                                                 (return ($builtIn pairs (<<<)))
                                                 end))
               ))

   (cons "pcall"
         (term (($ENV \[ "pcall" \]) = (function $pcall (<<<)
                                                 (return ($builtIn pcall (<<<)))
                                                 end))
               ))

   (cons "print"
         (term (($ENV \[ "print" \]) = (function $print (<<<)
                                                 (return ($builtIn print (<<<)))
                                                 end))
               ))

   (cons "rawequal"
         (term (($ENV \[ "rawequal" \]) = (function $rawequal (<<<)
                                                    (return ($builtIn rawequal (<<<)))
                                                    end))
               ))

   (cons "rawget"
         (term (($ENV \[ "rawget" \]) = (function $rawget (<<<)
                                                  (return
                                                   ($builtIn rawget
                                                             (<<<)))
                                                  end))
               ))

   (cons "rawset"
         (term (($ENV \[ "rawset" \]) = (function $rawset (<<<)
                                                  (return
                                                   ($builtIn rawset
                                                             (<<<)))
                                                  end))
               ))

   (cons "rawlen"
         (term (($ENV \[ "rawlen" \]) = (function $rawlen (<<<)
                                                  (return ($builtIn rawlen (<<<)))
                                                  end))
               ))

   (cons "select"
         (term (($ENV \[ "select" \]) = (function $select (index <<<)
                                                  (return ($builtIn select (index <<<)))
                                                  end))
               ))

   (cons "setmetatable"
         (term (($ENV \[ "setmetatable" \]) = (function $setmetatable (<<<)
                                                        (return ($builtIn setmetatable (<<<)))
                                                        end))
               ))

   (cons "tonumber"
         (term (($ENV \[ "tonumber" \]) = (function $tonumber (<<<)
                                                    (return ($builtIn tonumber (<<<)))
                                                    end))
               ))

   (cons "tostring"
         (term (($ENV \[ "tostring" \]) = (function $tostring (<<<)
                                                    (return ($builtIn tostring (<<<)))
                                                    end))
               ))

   (cons "type"
         (term (($ENV \[ "type" \]) = (function $type (<<<)
                                                (return ($builtIn type (<<<)))
                                                end))
               ))

   (cons "xpcall"
         (term (($ENV \[ "xpcall" \]) = (function $xpcall (<<<)
                                                  (return ($builtIn xpcall (<<<)))
                                                  end))
               ))

   (cons "_G"
         (term (($ENV \[ "_G" \]) = $ENV)
               ))

   
;                                               
;                                               
;                                               
;         ;           ;                         
;         ;           ;                         
;         ;           ;                         
;     ;;; ;    ;;;    ; ;;;    ;     ;    ;;; ; 
;    ;   ;;   ;   ;   ;;   ;   ;     ;   ;   ;; 
;   ;     ;  ;     ;  ;     ;  ;     ;  ;     ; 
;   ;     ;  ;     ;  ;     ;  ;     ;  ;     ; 
;   ;     ;  ;;;;;;;  ;     ;  ;     ;  ;     ; 
;   ;     ;  ;        ;     ;  ;     ;  ;     ; 
;    ;   ;;   ;    ;  ;;   ;   ;;   ;;   ;   ;; 
;     ;;; ;    ;;;;   ; ;;;     ;;;; ;    ;;; ; 
;                                             ; 
;                                        ;   ;; 
;                                         ;;;;  
;                                               
   (cons "debug"
         (term (($ENV \[ "debug" \]) = (\{ \}))
               ))

   (cons "debug.setmetatable"
         (term ((($ENV \[ "debug" \]) \[ "setmetatable" \])
                = (function $debug.setmetatable (<<<)
                            (return ($builtIn debug.setmetatable (<<<)))
                            end))
               ))
                                               
   ;                           ;      
   ;                           ;      
   ;                     ;     ;      
   ;                     ;     ;      
   ;   ;;;;;;;   ;;;   ;;;;;;  ; ;;;  
   ;   ;  ;  ;  ;   ;    ;     ;;   ; 
   ;   ;  ;  ;      ;    ;     ;    ; 
   ;   ;  ;  ;  ;;;;;    ;     ;    ; 
   ;   ;  ;  ; ;    ;    ;     ;    ; 
   ;   ;  ;  ; ;   ;;    ;     ;    ; 
   ;   ;  ;  ;  ;;; ;     ;;;  ;    ; 
   ;                                  
   ;                                  
   ;                                  
   (cons "math"
         (term (($ENV \[ "math" \]) = (\{ \}))
               ))

   (cons "math.abs"
         (term ((($ENV \[ "math" \]) \[ "abs" \]) = (function $math.abs (<<<)
                                                    (return ($builtIn math.abs (<<<)))
                                                    end))
               ))
   
   (cons "math.acos"
         (term ((($ENV \[ "math" \]) \[ "acos" \]) = (function $math.acos (<<<)
                                                     (return ($builtIn math.acos (<<<)))
                                                     end))
               ))

   (cons "math.asin"
         (term ((($ENV \[ "math" \]) \[ "asin" \]) = (function $math.asin (<<<)
                                                     (return ($builtIn math.asin (<<<)))
                                                     end))
               ))

   (cons "math.atan"
         (term ((($ENV \[ "math" \]) \[ "atan" \]) = (function $math.atan (<<<)
                                                     (return ($builtIn math.atan (<<<)))
                                                     end))
               ))

   (cons "math.ceil"
         (term ((($ENV \[ "math" \]) \[ "ceil" \]) = (function $math.ceil (<<<)
                                                               (return ($builtIn math.ceil (<<<)))
                                                               end))
               ))

   (cons "math.cos"
         (term ((($ENV \[ "math" \]) \[ "cos" \]) = (function $math.cos (<<<)
                                                              (return ($builtIn math.cos (<<<)))
                                                              end))
               ))

   (cons "math.cosh"
         (term ((($ENV \[ "math" \]) \[ "cosh" \]) = (function $math.cosh (<<<)
                                                               (return ($builtIn math.cosh (<<<)))
                                                               end))
               ))

   (cons "math.deg"
         (term ((($ENV \[ "math" \]) \[ "deg" \]) = (function $math.deg (<<<)
                                                              (return ($builtIn math.deg (<<<)))
                                                              end))
               ))

   (cons "math.exp"
         (term ((($ENV \[ "math" \]) \[ "exp" \]) = (function $math.exp (<<<)
                                                              (return ($builtIn math.exp (<<<)))
                                                              end))
               ))

   (cons "math.floor"
         (term ((($ENV \[ "math" \]) \[ "floor" \]) = (function $math.floor (<<<)
                                                                (return ($builtIn math.floor (<<<)))
                                                                end))
               ))
   
   (cons "math.fmod"
         (term ((($ENV \[ "math" \]) \[ "fmod" \]) = (function $math.fmod (<<<)
                                                     (return ($builtIn math.fmod (<<<)))
                                                     end))
               ))
   
   (cons "math.huge"
         (term ((($ENV \[ "math" \]) \[ "huge" \]) = +inf.0)
               ))
   
   (cons "math.log"
         (term ((($ENV \[ "math" \]) \[ "log" \]) = (function
                                                     $math.log (<<<)
                                                     (return ($builtIn math.log (<<<)))
                                                     end))
               ))
   
   (cons "math.max"
         (term ((($ENV \[ "math" \]) \[ "max" \]) = (function
                                                     $math.max (<<<)
                                                     (return ($builtIn math.max (<<<)))
                                                     end))
               ))
   
   (cons "math.modf"
         (term ((($ENV \[ "math" \]) \[ "modf" \]) = (function
                                                      $math.modf (<<<)
                                                      (return ($builtIn math.modf (<<<)))
                                                      end))
               ))
   
   (cons "math.pi"
         (term ((($ENV \[ "math" \]) \[ "pi" \]) = ,pi)
               ))
   
   (cons "math.rad"
         (term ((($ENV \[ "math" \]) \[ "rad" \]) = (function
                                                     $math.rad (<<<)
                                                     (return ($builtIn math.rad (<<<)))
                                                     end))
               ))
   
   (cons "math.sin"
         (term ((($ENV \[ "math" \]) \[ "sin" \]) = (function
                                                     $math.sin (<<<)
                                                     (return ($builtIn math.sin (<<<)))
                                                     end))
               ))
   
   (cons "math.sinh"
         (term ((($ENV \[ "math" \]) \[ "sinh" \]) = (function $math.sinh (<<<)
                                                               (return ($builtIn math.sinh (<<<)))
                                                               end))
               ))
   
   (cons "math.sqrt"
         (term ((($ENV \[ "math" \]) \[ "sqrt" \]) = (function $math.sqrt (<<<)
                                                               (return ($builtIn math.sqrt (<<<)))
                                                               end))
               ))
   
   (cons "math.tan"
         (term ((($ENV \[ "math" \]) \[ "tan" \]) = (function $math.tan (<<<)
                                                              (return ($builtIn math.tan (<<<)))
                                                              end))
               ))
   
   (cons "math.tanh"
         (term ((($ENV \[ "math" \]) \[ "tanh" \])
                = (function $math.tanh (<<<)
                            (return ($builtIn math.tanh (<<<)))
                            end))
               ))
   ;                         
   ;                         
   ;   ;                                                          
   ;   ;                           ;                              
   ;   ;                           ;                              
   ;   ;                           ;                              
   ;   ;                           ;                              
   ;   ;   ;;;;;     ;;;     ;;;   ;   ;     ;;;    ;;;;;   ;;;;  
   ;   ;   ;;  ;;   ;   ;   ;   ;  ;  ;     ;   ;  ;;  ;;  ;;  ;; 
   ;   ;   ;    ;       ;  ;       ; ;          ;  ;    ;  ;    ; 
   ;   ;   ;    ;   ;;;;;  ;       ;;;      ;;;;;  ;    ;  ;;;;;; 
   ;   ;   ;    ;  ;    ;  ;       ;  ;    ;    ;  ;    ;  ;      
   ;   ;   ;;  ;;  ;   ;;   ;   ;  ;   ;   ;   ;;  ;;  ;;  ;;   ; 
   ;   ;   ;;;;;    ;;; ;    ;;;   ;    ;   ;;; ;   ;;; ;   ;;;;  
   ;   ;   ;                                            ;         
   ;   ;   ;                                        ;   ;         
   ;   ;   ;                                         ;;;          
   ;
   (cons "require"
         (term (($ENV \[ "require" \])
                = (function $require (<<<)
                            (return ($builtIn require (<<<)))
                            end))
               ))
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

   (cons "string"
         (term (($ENV \[ "string" \]) = (\{ \}))
               ))
   
   (cons "string.dump"
         (term ((($ENV \[ "string" \]) \[ "dump" \])
                = (function $string.dump (<<<)
                            (return ($builtIn string.dump (<<<)))
                            end))
               ))

   (cons "string.len"
         (term ((($ENV \[ "string" \]) \[ "len" \])
                = (function $string.len (<<<)
                            (return ($builtIn string.len (<<<)))
                            end))
               ))

   (cons "string.rep"
         (term ((($ENV \[ "string" \]) \[ "rep" \])
                = (function $string.rep (<<<)
                            (return ($builtIn string.rep (<<<)))
                            end))
               ))

   (cons "string.reverse"
         (term ((($ENV \[ "string" \]) \[ "reverse" \])
                = (function $string.reverse (<<<)
                            (return ($builtIn string.reverse (string)))
                            end))
               ))

   (cons "string.sub"
         (term ((($ENV \[ "string" \]) \[ "sub" \])
                = (function $string.sub (<<<)
                            (return ($builtIn string.sub (<<<)))
                            end))
               ))
                         
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

   (cons "table"
         (term (($ENV \[ "table" \]) = (\{ \}))
               ))

   (cons "table.concat"
         (term ((($ENV \[ "table" \]) \[ "concat" \])
                = (function $table.concat (<<<)
                            (return ($builtIn table.concat (<<<)))
                            end))
               ))

   (cons "table.pack"
         (term ((($ENV \[ "table" \]) \[ "pack" \])
                = (function $table.pack (<<<)
                            (return ($builtIn table.pack (<<<)))
                            end))
               ))
   
   (cons "table.unpack"
         (term ((($ENV \[ "table" \]) \[ "unpack" \])
                = (function $table.unpack (<<<)
                            (return ($builtIn table.unpack (<<<)))
                            end))
               ))

   )
  )

(provide services)

; Definition of the execution environment as an evaluation context.
(define (create_exec$ENV avail_servs servs_selected)
  (append
   (term (() : () : ))

   (if (> (length servs_selected) 0)
       
       (list 
        (append
         ;(term (local $ENV = (\{ \}) in))
         (term (local $ENV = (\{ \}) in))

         (list (append (foldl (lambda (serv_name accum) (append accum
                                                                (list
                                                                 (dict-ref avail_servs serv_name))))
                              (list)
                              servs_selected)
                       ; NOTE: this is done in order to manage, in a simple way, the cases where
                       ; we plug into the hole a concatenation of statements, and not some other
                       ; kind of statement. This could lead to some errors, like when the concatenated
                       ; statements are function calls. An the resulting phrase could be incorrectly
                       ; interpreted as a function call: the previous assignment to the $ENV table
                       ; result in a tuple value, and the whole phrase could be seen as a function call.
                       (list (term (($ENV \[ "_G" \]) = $ENV)))
                       (list (term (local $dummyVar = nil in hole end)))
                       ;(list (term hole))
                       ))
         (term (end))))

       (list
        (append
         (term (local $ENV = (\{ \}) in))

         (list (append
                (list (term (($ENV \[ "_G" \]) = $ENV)))
                
                (list (term (local $dummyVar = nil in hole end)))))

         (term (end))))

       )))

(define (plugIntoExecutionEnvironment avail_servs servs_selected s)
  (term (in-hole
         ,(create_exec$ENV avail_servs servs_selected)
         ,s)))
         
(provide plugIntoExecutionEnvironment)

; Empty execution environment for easy testing of...ideas.
(define (plugIntoEmptyExecEnv s)
  (term (in-hole 
         (() : () : hole)
         ,s))
  )

(provide plugIntoEmptyExecEnv)