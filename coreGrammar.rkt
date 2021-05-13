#lang racket

(require redex)

(define-language core-lang
  ; terms as compiled from Lua code
  [sing \;
        break
        (return e ...)
        ($statFCall e (e ...))
        ($statFCall e : Name (e ...))
        (var_1 var_2 ... = e ...)
        (do s end)
        (if e then s else s end)
        (while e do s end)
        ($iter e do s end)
        (local Name_1 Name_2 ... = e ... in s end)]

  ; Lua's block of code: it helps to avoid an ambiguity in the grammar, between
  ; funcalls and concat. of stats
  [s sing
     (sing_1 sing_2 sing_3 ...)]

  [v nil Boolean Number String]

  [Boolean true false]
    
  ; Number represents real (double-precision floating-point) numbers
  [Number real]
  
  [String string]

  ; difference between statements and expressions is present also at a semantics
  ; level: eg., tuples' semantics is defined taking into account if they appear
  ; as an expression or as a statement, and the same with funcall
  [e v
     <<<
     var
     (\( e \))
     ($builtIn builtinserv (e ...))
     tableconstructor
     (e binop e)
     (unop e)
     functiondef
     (e (e ...))
     (e : Name (e ...))]
  
  ; identifiers of variables and refs., to ease the definition of several
  ; substitution functions
  [id Name
      <<<]

  [parameters (Name ...)
              (Name ... <<<)]

  ; This syntactic category is added to ease meta-functions' definitions. 
  [functiondef (function Name parameters s end)]

  ; Built-in services' names, for use by the random generator of terms
  [builtinserv assert
               collectgarbage
               error
               getmetatable
               ipairs
               load
               loadfile
               next
               pairs
               pcall
               print
               rawequal
               rawget
               rawlen
               rawset
               select
               setmetatable
               tonumber
               tostring
               type
               xpcall
               ; debug
               debug.setmetatable
               ; math
               math.abs
               math.acos
               math.asin
               math.atan
               math.ceil
               math.cos
               math.cosh
               math.deg
               math.exp
               math.floor
               math.fmod
               math.log
               math.max
               math.modf
               math.rad
               math.sin
               math.sinh
               math.sqrt
               math.tan
               math.tanh
               ; package
               require
               ; string
               string.dump
               string.len
               string.rep
               string.reverse
               string.sub
               ; table
               table.concat
               table.insert
               table.pack
               table.unpack]

  ; tables
  [field (\[ e \] = e)
         ; we need to allow fields like this
         e]

  [tableconstructor (\{ field ... \})]
  
  
  ; primitive operators
  [arithop + - * / ^ %]
  
  [relop < <= > >=]

  ; Not short-circuit binop
  [strictbinop arithop relop == ..]
  
  [binop strictbinop and or]
  
  [unop - not \#]
  
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]
  
  [var Name 
       (e \[ e \])])

(provide core-lang)