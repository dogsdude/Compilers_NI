#lang racket

(require "envEXAMPLE.rkt"
         ;"typesEXAMPLE.rkt"
         "project2.rkt"
         )

(provide (all-defined-out))

(define escape-env (empty-env))

(struct escape-info (escape? level)#:transparent #:mutable)


;;; public facing function
;;; increase level when we go inside function definition

(define (escape-visitor ast)
  (escape-visitor-helper ast 0))

;;; Worry about things with multiple parts, places where we define things
;;; Assume typechecking has already happened!
(define (escape-visitor-helper ast level)
  (match (first ast)
    [(NumExpr _)    (escape-env)]
    
    [(BoolExpr expr1 op expr2) (escape-visitor-helper expr1 level)
                               (escape-visitor-helper expr2 level)]
    [(MathExpr expr1 op expr2) (escape-visitor-helper expr1 level)
                               (escape-visitor-helper expr2 level)]
    [(LogicExpr expr1 op expr2) (escape-visitor-helper expr1 level)
                               (escape-visitor-helper expr2 level)]
    [(VarDecl type id expr)    (extend-env escape-env id (escape-info #f level))
                               (escape-visitor-helper expr level)]
    [(VarExpr name)            (let ([var (apply-env escape-env name)])
                                 (cond
                                   [(not (equal? #f var)) (error "Var does not exist" var)]
                                   [(equal? level (escape-info-level var)) escape-env]
                                   [else (set-escape-info-escape?! var #t)]))]))
    