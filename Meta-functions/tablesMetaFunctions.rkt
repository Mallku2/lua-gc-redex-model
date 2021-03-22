#lang racket
(require redex
         "../grammar.rkt"
         "./grammarMetaFunctions.rkt"
         "./delta.rkt")

; operations that deals with tables and their internal representation

; given an evaluated table that can contains fields with no key specified,
; then addKeys adds the corresponding numeric values, as the semantics
; of Lua 5.2 (seems to) dictates
(define-metafunction ext-lang
  addKeys : evaluatedtable -> evaluatedtable

  [(addKeys (\{ efield_1 ... \}))
   (\{ efield_5 ... \})
   
   ; from "efield ..." extract fields of the form "v"
   (where (v ...) ,(filter (lambda (field)
                             (is_v? field))
                           (term (efield_1 ...))))

   ; get length of (v ...)
   (where Number ,(length (term (v ...))))

   ; add numeric keys
   (where (efield_2 ...), (map (lambda (key value)
                                 (append (term (\[ ))
                                         (list key)
                                         (term ( \] = ))
                                         (list value)))
                               
                               ; list of numeric keys
                               (build-list (term Number)
                                           (lambda (nmbr) (+ nmbr 1)))

                               ; list of values.
                               (term (v ...))))

   ; from "efield_1 ...", extract fields of the form "[ key ] = value"
   ; we discrad fields with numeric key in [1; Number]
   ; Lua seems to give priority to numeric fields without key, over
   ; fields with numeric key
   (where (efield_3 ...)
          ,(filter (lambda (field)
                     (redex-match? ext-lang
                                   (side-condition (|[| v_1 |]| = v_2)
                                                   (or (not (is_number? (term v_1)))
                                                       (or (not (exact? (term v_1)))
                                                           (< (term v_1) 1)
                                                           (> (term v_1)
                                                              (term Number)))))
                                   (term ,field)))
                   (term (efield_1 ...))))
   
   ; delete nil valued fields, or fields with key nil or nan
   (where (efield_4 ... ) ,(filter (lambda (field)
                                     (not (redex-match ext-lang
                                                       (side-condition
                                                        (\[ v_1 \] = v_2)
                                                        (or (is_nil? (term v_1))
                                                            (equal? (term v_1)
                                                                    +nan.0)
                                                            (is_nil? (term v_2))))
                                                       field)))
                                   (term (efield_3 ... efield_2 ...))))

   ; delete repeated fields, giving priority to the last fields
   (where (efield_5 ...) (remove_rep_fields (efield_4 ...)))]

  ; default case: empty table constructor
  [(addKeys evaluatedtable)
   evaluatedtable])

(provide addKeys)

; remove fields with repeated key, in an evaluated table
; in Lua, the last filds have priority over the firsts
(define-metafunction ext-lang
  remove_rep_fields : (field ...) -> (field ...)

  [(remove_rep_fields ())
   ()]

  [(remove_rep_fields ((\[ v_1 \] = v_2) (\[ v_3 \] = v_4) ...))
   ((\[ v_1 \] = v_2) efield ...)

   ; field is not repeated in what's left
   (side-condition (not (memf (lambda (arg)
                                (equal? (term (δ == v_1 ,arg))
                                        (term true)))
                              (term (v_3 ...)))))

   (where (efield ...) (remove_rep_fields ((\[ v_3 \] = v_4) ...)))]

  ; default: first field repeated
  [(remove_rep_fields (_ field ...))
   (remove_rep_fields (field ...))]
  )