#lang racket

(require "llvm-emitter-skeleton.rkt"
         "names.rkt"
         (prefix-in t: "types.rkt")
         "niparser.rkt"
         "typecheck.rkt"
         "errors.rkt"
         "log.rkt")

(provide (all-defined-out))

(define (trans str)
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse error")
        (let ([ty (typecheck-ast ast)])
         (if (error-generated?)
             (error "cannot translate due to type error")
             (begin
               (ast->llvm (first ast))
               (finish-emission)))))))

  
(define (translate-str str)
  ; clear the errors first
  (clear-errors)
  (clear-writers)
  (let ([ast (parse-str str)])
    (if (error-generated?)
        (error "cannot translate due to scan or parse errors")
        (let ([ty (typecheck-ast ast)])
          (if (error-generated?)
              (error "cannot translate due to type error")
              (begin
                (translate-ast ast)))))))

(define (translate-ast ast)
  ; begin with the prelude
  (emit-header)
  (emit-main-header)
  (ast->llvm (first ast))
  (emit-main-trailer)
  (finish-emission))

(define (ast->llvm ast)
  (match ast
    ; deal with lists, like in let expressions
    ['() '()]
    [(cons first rest) (begin (ast->llvm first) (ast->llvm rest))]
    
    ; integer literals
    [(NumExpr val) (numexpr->llvm ast val)]

    ; boolean literals
    [(BoolVal val) (boolexpr->llvm ast val)]

    ;[(StringExpr val) (stringexpr->llvm ast val)]

    ; variable declarations!
    ;[(VarDecl _ _ _) (vardecl->llvm ast)]
    
    ; function calls
    ;[(FuncallExpr _ _) (funcall->llvm ast)]
       
    ; variable expressions
    ;[(VarExpr _) (var->llvm ast)]

    ; let expressions--need these for any declarations to work!
    ;[(LetExpr _ _) (letexpr->llvm ast)]

    ; math expressions
    [(MathExpr _ _ _) (mathexpr->llvm ast)]

    ; logic expressions
    [(LogicExpr _ _ _) (logicexpr->llvm ast)]
    
    [_ (error "Translation node " ast " not implemented yet!")]))        

; emits a numeric literal
(define (numexpr->llvm node val)
  ; literal nums can go in registers
  (let ([result (emit-math 'add val "0")])
    (add-note node 'result result)
    result))

; emits a boolean literal
(define (boolexpr->llvm node val)
  (let* ([boolval (if val "1" "0")]
         [result (emit-math 'add boolval "0")])
    (add-note node 'result result)
    result))

; emits a math expression
(define (mathexpr->llvm node)
    (let ([expr1 (MathExpr-expr1 node)]
          [expr2 (MathExpr-expr2 node)])
      (let ([result1 (ast->llvm expr1)]
            [result2 (ast->llvm expr2)])
        (emit-math (MathExpr-op node)
                   result1
                   result2))))

; emits a logic expression
(define (logicexpr->llvm node)
    (let ([expr1 (LogicExpr-expr1 node)]
          [expr2 (LogicExpr-expr2 node)])
      (let ([result1 (ast->llvm expr1)]
            [result2 (ast->llvm expr2)])
        (emit-logic (LogicExpr-op node)
                   result1
                   result2))))