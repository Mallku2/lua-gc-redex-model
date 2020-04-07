#lang racket
(require redex
         "../grammar.rkt"
         )

; Operations that deals with tables and their internal representation

; Given an evaluated table that can contains fields with no key specified,
; then addKeys adds the corresponding numeric values, as the semantics
; of Lua 5.2 dictates
(define-metafunction ext-lang
  addKeys : evaluatedtable -> evaluatedtable

  [(addKeys (\{ efield ... \}))
   ,(append (term (\{ ))
            (term any_3)
            (term any_4)
            (term ( \})))
   
   ; From "efield ..." extract fields of the form "value"
   (where any_1 ,(filter (lambda (field)
                           (is_v? field)
                           )
                         (term (efield ...))))

   (where Number ,(length (term any_1)))

   ; Add keys
   (where any_2, (map (lambda (key value)
                        (append (term (\[ ))
                                (list key)
                                (term ( \] = ))
                                (list value)))

                      ; List of numeric keys
                      (build-list (term Number)
                                  (lambda (nmbr) (+ nmbr 1)))

                      ; List of values.
                      (term any_1)))

   ; Delete nil valued fields
   (where any_3 ,(filter (lambda (field)
                            (not (redex-match ext-lang
                                        (\[ v \] = nil)
                                        field)))
                         (term any_2)))

   ; From "efield ...", extract fields of the form "[ key ] = value"
   (where any_4 ,(filter (lambda (field)
                           ; Omit fields with numeric keys in [1; Number]
                           ((lambda (match)
                              (if match
                                  ((lambda (key)
                                     (if (number? key)
                                         (if (and (<= 1 key)
                                                  (<= key (term Number)))
                                             #f
                                             #t)
                                         #t)
                                     )
                                   ; Extract the key
                                   (bind-exp (list-ref (match-bindings
                                                       (list-ref match 0))
                                                      0)))
                                  #f))
                            ; Pass fields of the form (\[ any_1 \] = any_2) 
                            (redex-match ext-lang
                                         (\[ any_1 \] = any_2)
                                         field)))
                         (term (efield ...))))]

  ; Default case: empty table constructor
  [(addKeys evaluatedtable)
   evaluatedtable])

(provide addKeys)

; Determines if a given table is a sequence
(define-metafunction ext-lang
  checkSequence : tableconstructor -> any
  
  [(checkSequence tableconstructor)
   (checkSequenceAux tableconstructor 1 (maxKeyNumber tableconstructor))])

(provide checkSequence)

(define-metafunction ext-lang
  checkSequenceAux : tableconstructor Number Number -> any
  
  [(checkSequenceAux tableconstructor Number Number_2)
   (checkSequenceAux tableconstructor Number_3 Number_2)
   
   (side-condition (<= (term Number) (term Number_2)))
   (side-condition (term (keyBelongsTo? tableconstructor Number)))
   
   (where Number_3 ,(+ (term Number) (term 1)))]
  
  [(checkSequenceAux tableconstructor Number Number_2)
   #f
   
   (side-condition (<= (term Number) (term Number_2)))
   (side-condition (not (term (keyBelongsTo? tableconstructor Number))))]
  
  [(checkSequenceAux tableconstructor Number Number_2)
   #t
   (side-condition (> (term Number) (term Number_2)))])

; Determines the maximum natural n such that there is a field with n as key.
; If such n exist, then it is returned. Otherwise it returns 0
(define-metafunction ext-lang
  maxKeyNumber : tableconstructor -> Number
  [(maxKeyNumber (\{ field ... \}))
   (maxKeyNumberAux (field ...) 0)])

(provide maxKeyNumber)

; Auxiliar meta-function used by maxKeyNumber that actually performs
; the task, searching the max key number, begining by some number n.
; ret = (maxKeyNumberAux table n)
; POS : {ret is the maximum natural number, greater or equal to n, such
;        that there is a field in table with key m or ret = n, otherwise}
(define-metafunction ext-lang
  maxKeyNumberAux : (field ...) Number -> Number
  
  [(maxKeyNumberAux () Number)
   Number]
  
  [(maxKeyNumberAux ((\[ Number \] = v) field ...) Number_2)
   (maxKeyNumberAux (field ...) Number_2)
   (side-condition (<= (term Number) (term Number_2)))]
  
  [(maxKeyNumberAux ((\[ Number \] = v) field ...) Number_2)
   (maxKeyNumberAux (field ...) Number)
   (side-condition (> (term Number) (term Number_2)))]
  
  [(maxKeyNumberAux ((\[ v \] = v_2) field ...) Number)
   (maxKeyNumberAux (field ...) Number)])

; To ease the comparison of the numeric keys of a table
(define-metafunction ext-lang
  extractNumericField : (field ...) Number -> any
  
  [(extractNumericField (field ...) Number)
   ,(filter (lambda (field)
              ; Take each field that equals to
              ; (\[ v \] = any), according to delta
              ((lambda (match)
                 (if match
                     
                     ; There must be just one match structure
                     ((lambda (bindings)
                        ; Compare the keys
                        (if (= (bind-exp (list-ref bindings 1)) (term Number))
                            #t
                            #f))
                      ; Pass the only match structure obtained
                      (match-bindings (list-ref match 0)))
                     
                     #f))
               ; Extract each component of the field
               (redex-match ext-lang
                            (\[ Number \] = any)
                            field)))
            (term (field ...)))])

; To help with the gc implementation
(define-metafunction ext-lang
  extractObjrefs : (field ...) -> (objref ...)

  [(extractObjrefs ())
   ()]
  
  [(extractObjrefs ((\[ objref_1 \] = objref_2) field ...))
   ,(append (term (objref_1 objref_2))
            (term (extractObjrefs (field ...))))]


  [(extractObjrefs ((\[ objref \] = v) field ...))
   ,(append (term (objref))
            (term (extractObjrefs (field ...))))]

  [(extractObjrefs ((\[ v \] = objref) field ...))
   ,(append (term (objref))
            (term (extractObjrefs (field ...))))]

  [(extractObjrefs ((\[ v_1 \] = v_2) field ...))
   (extractObjrefs (field ...))]
  )

(provide extractObjrefs)

(define-metafunction ext-lang
  deleteTableField : objref v θ -> θ

  [(deleteTableField objref v_1
                     (osp_1 ...
                      (objref ((\{ field_1 ... (\[ v_1 \] = v_2) field_2 ... \}) any ...))
                      osp_2 ...))
   (osp_1 ...
    (objref ((\{ field_1 ... field_2 ... \}) any ...))
    osp_2 ...)]
  )

(provide deleteTableField)