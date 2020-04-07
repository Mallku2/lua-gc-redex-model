#lang racket
(require redex
         "./grammar.rkt"
         "./Relations/fullProgs.rkt")
; Dictionary as a list of pairs of the form (service's name, code), for easy
; construction of a suitable execution environment.
(define services
  (list
   ; Basic functions
   (cons "assert"
         (term (($ENV \[ "assert" \]) = (function $assert (v message <<<)
                                                  (return ($builtIn assert (v message <<<)))
                                                  end))
               ))

   (cons "collectgarbage"
         (term (($ENV \[ "collectgarbage" \]) = (function $collectgarbage (opt arg)
                                                          (return ($builtIn collectgarbage (opt arg)))
                                                          end))
               ))

   (cons "error"
         (term (($ENV \[ "error" \]) = (function $error (message level)
                                                 (return ($builtIn error (message level)))
                                                 end))
               ))

   (cons "getmetatable"
         (term (($ENV \[ "getmetatable" \]) = (function $getmetatable (object)
                                                        (return
                                                         ($builtIn getmetatable
                                                                   (object)))
                                                        end))
               ))

   (cons "load"
         (term (($ENV \[ "load" \]) = (function $load (ld source mode env)
                                                (return ($builtIn load (ld source mode env)))
                                                end))
               ))

   (cons "loadfile"
         (term (($ENV \[ "loadfile" \]) = (function $loadfile (filename mode env)
                                                    (return ($builtIn loadfile (filename mode env)))
                                                    end))
               ))

   (cons "ipairs"
         (term (($ENV \[ "ipairs" \]) = (function $ipairs (<<<)
                                                  (return ($builtIn ipairs (<<<)))
                                                  end))
               ))

   (cons "next"
         (term (($ENV \[ "next" \]) = (function $next (table index)
                                                (return ($builtIn next (table index)))
                                                end))
               ))

   (cons "pairs"
         (term (($ENV \[ "pairs" \]) = (function $pairs (<<<)
                                                 (return ($builtIn pairs (<<<)))
                                                 end))
               ))

   (cons "pcall"
         (term (($ENV \[ "pcall" \]) = (function $pcall (v <<<)
                                                 (return ($builtIn pcall (v <<<)))
                                                 end))
               ))

   (cons "print"
         (term (($ENV \[ "print" \]) = (function $print (<<<)
                                                 (return ($builtIn print (<<<)))
                                                 end))
               ))

   (cons "rawequal"
         (term (($ENV \[ "rawequal" \]) = (function $rawequal (v1 v2)
                                                    (return ($builtIn rawequal (v1 v2)))
                                                    end))
               ))

   (cons "rawget"
         (term (($ENV \[ "rawget" \]) = (function $rawget (table index)
                                                  (return
                                                   ($builtIn rawget
                                                             (table index)))
                                                  end))
               ))

   (cons "rawset"
         (term (($ENV \[ "rawset" \]) = (function $rawset (table index value)
                                                  (return
                                                   ($builtIn rawset
                                                             (table index value)))
                                                  end))
               ))

   (cons "rawlen"
         (term (($ENV \[ "rawlen" \]) = (function $rawlen (v)
                                                  (return ($builtIn rawlen (v)))
                                                  end))
               ))

   (cons "select"
         (term (($ENV \[ "select" \]) = (function $select (<<<)
                                                  (return ($builtIn select (<<<)))
                                                  end))
               ))

   (cons "setmetatable"
         (term (($ENV \[ "setmetatable" \]) = (function $setmetatable (<<<)
                                                        (return ($builtIn setmetatable (<<<)))
                                                        end))
               ))

   (cons "tonumber"
         (term (($ENV \[ "tonumber" \]) = (function $tonumber (e base)
                                                    (return ($builtIn tonumber (e base)))
                                                    end))
               ))

   (cons "tostring"
         (term (($ENV \[ "tostring" \]) = (function $tostring (v)
                                                    (return ($builtIn tostring (v)))
                                                    end))
               ))

   (cons "type"
         (term (($ENV \[ "type" \]) = (function $type (v)
                                                (return ($builtIn type (v)))
                                                end))
               ))

   (cons "xpcall"
         (term (($ENV \[ "xpcall" \]) = (function $xpcall (v_1 v_2 <<<)
                                                  (return ($builtIn xpcall (v_1 v_2 <<<)))
                                                  end))
               ))

   (cons "_G"
         (term (($ENV \[ "_G" \]) = $ENV)
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
         (term ((($ENV \[ "math" \]) \[ "abs" \]) = (function $math.abs (x)
                                                    (return ($builtIn math.abs (x)))
                                                    end))
               ))
   
   (cons "math.acos"
         (term ((($ENV \[ "math" \]) \[ "acos" \]) = (function $math.acos (x)
                                                     (return ($builtIn math.acos (x)))
                                                     end))
               ))

   (cons "math.asin"
         (term ((($ENV \[ "math" \]) \[ "asin" \]) = (function $math.asin (x)
                                                     (return ($builtIn math.asin (x)))
                                                     end))
               ))

   (cons "math.atan"
         (term ((($ENV \[ "math" \]) \[ "atan" \]) = (function $math.atan (x)
                                                     (return ($builtIn math.atan (x)))
                                                     end))
               ))

   (cons "math.ceil"
         (term ((($ENV \[ "math" \]) \[ "ceil" \]) = (function $math.ceil (x)
                                                               (return ($builtIn math.ceil (x)))
                                                               end))
               ))

   (cons "math.cos"
         (term ((($ENV \[ "math" \]) \[ "cos" \]) = (function $math.cos (x)
                                                              (return ($builtIn math.cos (x)))
                                                              end))
               ))

   (cons "math.cosh"
         (term ((($ENV \[ "math" \]) \[ "cosh" \]) = (function $math.cosh (x)
                                                               (return ($builtIn math.cosh (x)))
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
         (term ((($ENV \[ "math" \]) \[ "fmod" \]) = (function $math.fmod (y <<<)
                                                     (return ($builtIn math.fmod (y <<<)))
                                                     end))
               ))
   
   (cons "math.huge"
         (term ((($ENV \[ "math" \]) \[ "huge" \]) = +inf.0)
               ))
   
   (cons "math.log"
         (term ((($ENV \[ "math" \]) \[ "log" \]) = (function
                                                     $math.log (base <<<)
                                                     (return ($builtIn math.log (base <<<)))
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
                = (function $require (modname)
                            (return ($builtIn require (modname)))
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
                = (function $string.dump (func)
                            (return ($builtIn string.dump (func)))
                            end))
               ))

   (cons "string.len"
         (term ((($ENV \[ "string" \]) \[ "len" \])
                = (function $string.len (string)
                            (return ($builtIn string.len (string)))
                            end))
               ))

   (cons "string.rep"
         (term ((($ENV \[ "string" \]) \[ "rep" \])
                = (function $string.rep (string n sep)
                            (return ($builtIn string.rep (string n sep)))
                            end))
               ))

   (cons "string.reverse"
         (term ((($ENV \[ "string" \]) \[ "reverse" \])
                = (function $string.reverse (string)
                            (return ($builtIn string.reverse (string)))
                            end))
               ))

   (cons "string.sub"
         (term ((($ENV \[ "string" \]) \[ "sub" \])
                = (function $string.sub (string i j)
                            (return ($builtIn string.sub (string i j)))
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
                = (function $table.concat (list sep i j)
                            (return ($builtIn table.concat (list sep i j)))
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
                = (function $table.unpack (list i j)
                            (return ($builtIn table.unpack (list i j)))
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