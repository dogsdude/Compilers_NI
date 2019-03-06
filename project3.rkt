#lang racket

(require "envEXAMPLE.rkt"
         "project2.rkt"
  (prefix-in types: "typesEXAMPLE.rkt"))

(provide (all-defined-out))

;;;Intialize Type Environment to the
;;;    Empty Environment and then add
;;;    definitions for the Base Types
(define (init-typeEnv)
  (let ([tenv (empty-env)])
    (extend-env tenv 'int    (types:make-IntType))
    (extend-env tenv 'string (types:make-StringType))
    (extend-env tenv 'bool   (types:make-BoolType))
    (extend-env tenv 'peng   (types:make-PengType))
    (extend-env tenv 'void   (types:make-VoidType));;; NOT SURE ABOUT THIS ONE...
    
    ;test our x
    ;test our arraytype
    (extend-env tenv 'x       (types:make-IntType))
    (extend-env tenv 'foo     (types:make-ArrayType 'arr (types:make-StringType)))
))

(define typeEnv (make-parameter (init-typeEnv)))

(define (init-typechecker)
  (typeEnv (init-typeEnv))
  )
           
(define (tc-str str)
  (init-typechecker)
  (let ([ast (parse-str str)])
    ;;;This should take (first ast) - ParseStr should return a list
    (let ([ty (typecheck  (first ast) (typeEnv))])
          ty)))

(define (typecheck ast env)
  (let ([type-of-expr
         (match ast

           [(list _ ... _ ) (let ([first (first ast)]
                                  [rest  (rest ast)])
                              (cond
                                [(equal? rest '()) (typecheck first env)]
                                [else (typecheck first env) (typecheck rest env)]))]
           
           [(NumExpr _ ) (types:make-IntType)]
           [(StringExpr _ ) (types:make-StringType)]
           [(BoolVal _ ) (types:make-BoolType)]
           [(PengExpr) (types:make-PengType)]

           [(VarExpr name) (apply-env env (string->symbol name))]

           ;THIS WORKS!
           [(ArrayExpr name index)
            (let* ([t1 (typecheck index env)]
                  [t2 (apply-env env (string->symbol name))]
                  [elementType (types:ArrayType-element-type t2)])
              (cond [(equal? #f t2)           (error "no arraytype of this type which exists")]
                    [(types:IntType? t1) elementType]
                    [else (error "Index must be an int")]))]

           [(NoVal)   (types:make-VoidType)]
           
           [(BreakExpr)   (types:make-VoidType)]
           
           [(MathExpr e1 op e2)
            (let ([t1 (typecheck e1 env)]
                  [t2 (typecheck e2 env)])
              (cond [(and(types:IntType? t1)
                         (types:IntType? t2)) (types:make-IntType)]
                    [else (error "Types: " t1 " and " t2 " are not the same! Should both be int... ")])
              )]
           
           [(LogicExpr e1 op e2)
            (let ([t1 (typecheck e1 env)]
                  [t2 (typecheck e2 env)])
              (cond [(and (types:BoolType? t1)
                          (types:BoolType? t2)) (types:make-BoolType)]
                    [else (error "Types: " t1 " and " t2 " are not the same! Should both be bool... ")])
              )]
           
           [(BoolExpr e1 op e2)
            (let ([t1 (typecheck e1 env)]
                  [t2 (typecheck e2 env)])
              (cond [(and (equal? t1 t2)) (types:make-BoolType)]
                    [else (error "Types: " t1 " and " t2 " are not the same! Should both be the same... ")])
              )]
           
           [(VarDecl type name expr)
            (let ([t1 (if (equal? type #f) #f (apply-env env (string->symbol type))) ]
                  [t2 (typecheck expr env)])
              (cond [(and (equal? t1 t2)) (extend-env env (string->symbol name) t2)]
                    [(and (not (equal? t2 types:PengType)) (equal? type #f) (extend-env env (string->symbol name) t2))]
                    [else (error "Types: " t1 " and " t2 " are not the same! Should both be the same... ")])
              )]

           [(TypeField name type) (extend-env env (string->symbol name)) (typecheck type env)]

           [(NameType name kind '()) (let ([kind-t (apply-env env (string->symbol kind))]
                                          [l-exists (apply-env env (string->symbol name))])
                                     (cond
                                       [(equal? kind-t #f) (error "kind has to be an existing type" kind)]
                                       [l-exists (error "name already in use in another place" l-exists)]
                                       [else      (extend-env env (string->symbol name) kind-t) kind-t]))] 
            
           [(NameType name kind next) (let ([next-t (typecheck next env)]
                                            [kind-t (apply-env env (string->symbol kind))]
                                            [l-exists (apply-env env (string->symbol name))])
                                            (cond
                                              [(equal? kind-t #f) (error "kind has to be an existing type" kind)]
                                              [l-exists (error "name already in use in another place" l-exists)]
                                              [else (extend-env env (string->symbol name) kind-t) next-t]))]

           [(ArrayType name kind '()) (let* ([kind-t (apply-env env (string->symbol kind))]
                                            [l-exists (apply-env env (string->symbol name))]
                                            [arrtype (types:make-ArrayType '() kind-t )])
                                            (cond
                                              [(equal? kind-t #f) (error "kind has to be an existing type" kind)]
                                              [l-exists (error "name already in use in another place" l-exists)]
                                              [else (extend-env env (string->symbol name) arrtype) arrtype]))]
           
           [(ArrayType name kind next) (let* ([next-t (typecheck next env)]
                                             [kind-t (apply-env env (string->symbol kind))]
                                             [l-exists (apply-env env (string->symbol name))]
                                             [arrtype (types:make-ArrayType '() kind-t)])
                                             
                                         (cond
                                           [(equal? kind #f) (error "kind has to be an existing type" kind)]
                                           [l-exists (error "name already in use in another place" l-exists)]
                                           [else (extend-env (string->symbol name) arrtype) next-t]))]

           [(IfExpr test true false) (let*([test-t  (typecheck test env)]
                                           [true-t  (typecheck true env)]
                                           [false-t (if (equal? false '()) true-t (typecheck false env))])
                                       (cond
                                         [(not (types:BoolType? test-t)) (error "must be of type boolean" test-t)] 
                                         [(not (equal? true-t false-t)) (error "types must match" true-t false-t)]
                                         [else true-t]))]

           [(WhileExpr test body)  (let* ([test-t (typecheck test env)]
                                          [body-t (typecheck body env)])
                                     (cond
                                       [(not (types:BoolType? test-t)) (error "body must be of type boolean" body-t)]
                                       [else body-t]))]

           [(WithExpr id val pred body) (let* ([val-t (typecheck val env)]
                                              [new-env(extend-env (push-scope env) id val-t)]
                                              [pred-t(typecheck pred new-env)]
                                              [body-t(typecheck body new-env)])
                                          (cond
                                            [(not (equal? val-t pred-t)) (error "val and cond are not sim type" val-t pred-t)]
                                            [(not (and (types:IntType? val-t) (types:IntType? pred-t))) (error "values here must both be ints" val-t pred-t)]
                                            [else (pop-scope new-env) body-t]))]
                                          

           [(AssignmentExpr name expr) (let* ([left (typecheck name env)]
                                              [right(typecheck expr env)])
                                         (cond
                                           [(equal? left #f) (error "l-val not dec")]
                                           [(not (equal? left right)) (error "left and right types don't match")]
                                          [else right]))]

           [(FieldAssign name expr) (let* ([name-t (apply-env env (string->symbol name))]
                                           [expr-t (typecheck env expr)])
                                      (cond
                                        [(equal? name-t #f) (error "needs to be here")]
                                        [(equal? name-t expr-t) (error "need to be same")]
                                        [else name-t]))]

           ;Making a lot of assumptions assuming that other parts of this thing are working right now...
           [(LetExpr decs exprs) (let*
                                     ([new-env (push-scope env)]
                                      [decs-t (typecheck decs new-env)]
                                      [exprs-t(typecheck exprs new-env)])
                                     exprs-t)]

           [(NewArrayExpr typeid size init) (let* ([typeid-t (apply-env env (string->symbol typeid))]
                                                   [size-t (typecheck size env)]
                                                   [init-t (typecheck init env)])
                                              (cond
                                                [(equal? typeid-t #f) (error "Name does not exist in environment " typeid-t)]
                                                [(not (types:IntType? size-t)) (error "Size has to be an int " size-t)]
                                                [(not (equal? init-t (types:ArrayType-element-type typeid-t))) (error "These must be the same type" init-t typeid-t)]
                                                [else typeid-t]))]

           
                                                 
                                           
            
           #;[(NewArrayExpr name num-elements initial-val)
                          (let*
                              ([arrty (types:actual-type (apply-env typeEnv name))]
                               [countty (types:actual-type (typecheck num-elements typeEnv venv loop? level))]
                               [initty (types:actual-type (typecheck init-val typeEnv venv loop? level))])
                            (cond
                              [(not (types:ArrayType? arrty))
                               (begin (log-typeerror "~a must be an array type" ast name) (types:make-VoidType))]
                              [(not (types:IntType? countty))
                               (begin (log-typeerror "number of elements in an array must be an int type" ast) (types:make-VoidType))]
                              [(not (types:type =? initty (types:ArrayType-element-type arrty)))
                               (begin
                                 (log-typeerror "intialization value for array doesn't match type of array elements" ast)
                                 (types:make-VoidType))])
                            arrty)]
           
;            IntType
;            StringType
;            VoidType
;            BoolType
;            PengType
;            ArrayType
;            RecordType
;            NameType
           
           [_ (error "Type Checking Error")]
           )])
    type-of-expr))