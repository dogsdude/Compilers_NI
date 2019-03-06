#lang racket

(provide (all-defined-out))

;;; Types for Ni

;;; Base type, we don't ever actually build stuff from this (like object type)
(struct NiType ([actual #:mutable]) #:transparent
  #:guard (lambda (actual typename)
            (if (eq? typename 'NiType)
                (error "Cannot Instantiate NiType directly.")
                (if (or (eq? actual '())
                        (NiType? actual))
                    (values actual)
                    (raise-arguments-error typename
                                           "Can only instantiate with NiTypes or '()"
                                           "actual: " actual)))))

;;; Strings
(struct StringType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (lambda (ty port mode)
                                  (write-string "type:str" port)))])

(struct foo (x y) #:transparent)

;;;Voids
(struct VoidType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (lambda (ty port mode)
                                  (write-string "type:void" port)))])

;;;Int
(struct IntType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (lambda (ty port mode)
                                  (write-string "type:int" port)))])

;;;Bool
(struct BoolType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (lambda (ty port mode)
                                  (write-string "type:bool" port)))])

;;;Peng
(struct PengType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (lambda (ty port mode)
                                  (write-string "type:peng" port)))])

;;;NameType
(struct NameType NiType (name) #:transparent
  #:methods gen:custom-write [(define write-proc
                               (lambda (ty port mode)
                                 (let ([theprinter (case mode
                                                     [(#t) write]
                                                     [(#f) display]
                                                     [else (lambda (p port)
                                                                      (print p port mode))])])
                                   (write-string "<name: " port)
                                   (theprinter (NiType-actual ty) port)
                                   (write-string ">" port))))]
  #:guard (lambda (actual name tyname)
            (cond
              [(and (not (symbol? name))
                    (raise-arguments-error tyname "NameType name must be a symbol"
                                           "actual" actual
                                           "name" name))]
              [else (values actual name)])))

;;;ArrayType
(struct ArrayType NiType (name element-type
                               [label #:mutable #:auto]) #:transparent
  #:auto-value #f
  #:methods gen:custom-write [(define write-proc (lambda (ty port mode)
                                                   (let
                                                       ([theprinter (case mode
                                                                      [(#t) write]
                                                                      [(#f) display]
                                                                      [else (lambda (p port)
                                                                              (print p port mode))])])
                                                     (display "type:array[ " port)
                                                     (theprinter (ArrayType-element-type ty) port)
                                                     (display " ]" port))))])

;;;Functions for Building Types (Base Types)
(define (make-IntType)
  (IntType '()))

(define (make-StringType)
  (StringType '()))

(define (make-BoolType)
  (BoolType '()))

(define (make-PengType)
  (PengType '()))

(define (make-VoidType)
  (VoidType '()))

(define (make-ArrayType name etype)
  (ArrayType '() name etype))

(define (make-RecordType name fields)
  (RecordType '() name fields))


;;;RecordType
;;;NameTypePair (For the field of the record)
(struct RecordType NiType (name fields [label #:mutable #:auto]) #:transparent
  #:auto-value #f
  #:methods gen:custom-write [(define write-proc (lambda (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (display "t:rec{ " port)
                                                     (for-each (lambda (field)
                                                                 (theprinter field port)
                                                                 (display " " port)) (RecordType-fields ty))
                                                     (display "}" port))))])

(struct NameTypePair (type name) #:transparent
  
  #:methods gen:custom-write [(define write-proc (lambda (ty port mode)
                                                   (let ([theprinter (case mode
                                                                       [(#t) write]
                                                                       [(#f) display]
                                                                       [else (lambda (p port) (print p port mode))])])
                                                     (display "<" port)
                                                     (theprinter (NameTypePair-name ty) port)
                                                     (display ", " port)
                                                     (theprinter(NameTypePair-name ty) port)
                                                     (display ">" port))))]
  #:guard (lambda (type name typename)
            (cond [(and (or (VarValue? type) (NiType? type)) (symbol? name))
                   (values type name)]
                  [else
                   (raise-arguments-error "NameTypePair requires a VarValue/NiType and symbol"
                                          typename
                                          "type"type
                                          "name"name)])))


                                                     


;;;Values for Ni
;;;VarValue (for the Named Variables)
(struct VarValue (type
                   [read-only? #:mutable]
                   [level #:mutable]
                   [escape? #:mutable]
                   [offset #:mutable]
                   [result #:auto #:mutable]) #:transparent #:auto-value #f)

;;;FunValue (for the Named Functions)
(struct FunValue (name parameters return-type [label #:mutable #:auto]
                       [frame #:mutable #:auto])
  #:transparent #:auto-value #f)

(define (make-VarValue ty)
  (VarValue ty #f #f #f #f))

(define (actual-type ty)
  (let ([actual (NiType-actual ty)])
    (cond
      [(and (null? actual) (NameType? ty)) (error "NameType must have an actual by the time you call")]
      [(null? actual ty)]
      [else (actual-type actual)])))

