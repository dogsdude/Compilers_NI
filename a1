; Sam Lindsey, Assignment1, 1/14/2019

#lang racket
; a structure to hold a digit, this is used by the lexer
; a token type will be 'op, 'lparen, 'rparen, or 'digit or 'eof
; repr is a character representation
(struct token (type repr) #:transparent
   ; a guard is a function that 'tests' the values you put into the structure
   ; remember: racket is dynamically typed so you kinda have to check things to
   ; save yourself a ton of grief later (trust me)
   #:guard (λ (type repr struct-name)
     (if (not (is-token-type? type))
         (error "expected a proper token-type which is-token-type? returns true from, got" type)
         
         (if (and (not (eq? eof repr)) (not (char? repr)))
             (error "expected a string? or eof? for token-repr, got" repr)
             (values type repr)))))


; symbol -> bool
; returns true if the token matches the symbols 'op, 'lparen, 'rparen, 'digit
(define (is-token-type? t)
  (cond
    [(equal? t 'op ) #t]
    [(equal? t 'lparen) #t]
    [(equal? t 'rparen) #t]
    [(equal? t 'digit) #t]
    [(equal? t 'eof) #t]
    [else #f]
    )
  )

; input-port -> token
; returns the next input token from the input port
(define (get-next-token input-port)
  (let ([input (read-char input-port)])
    (cond
      [(equal? input #\+) (token 'op #\+)]
      [(equal? input #\*) (token 'op #\*)]
      [(equal? input #\0) (token 'digit #\0)]
      [(equal? input #\1) (token 'digit #\1)]
      [(equal? input #\2) (token 'digit #\2)]
      [(equal? input #\3) (token 'digit #\3)]
      [(equal? input #\4) (token 'digit #\4)]
      [(equal? input #\5) (token 'digit #\5)]
      [(equal? input #\6) (token 'digit #\6)]
      [(equal? input #\7) (token 'digit #\7)]
      [(equal? input #\8) (token 'digit #\8)]
      [(equal? input #\9) (token 'digit #\9)]
      [(equal? input #\() (token 'lparen #\()]
      [(equal? input #\)) (token 'rparen #\))]
      [(equal? input eof) (token 'eof input)]
      (else (raise-syntax-error #f
                                    (string-append "Unexpected syntax: " (string input) ))
            ))))
    

; string -> 0 argument function that returns the next token on the string
; this function creates a function that uses get-next-token on the string that was passed in,
; notice how we pass create the input by using open-input-string
(define (lexstr str)
 (let ([input (open-input-string str)])
 (λ () (get-next-token input))))

 ; value node for numbers
(struct ast-node (val) #:transparent)
; expression nodes for operators 
(struct ast-expr-node (operator left-child right-child) #:transparent)


; (() -> token) -> (ast-node | ast-expr-node)
; the parser takes a function (probably produced by lexstr) that
; lexes the contents of the input stream
(define (parser lex)
  (let* ([token (lex)]
         [type (token-type token)]
         [value (token-repr token)])
    (cond
      ([equal? type 'eof] '())
      ([equal? type 'digit] (ast-node (string->number (string value))))
      ([equal? type 'lparen] (parse-ast-expr-node lex))
      (else (error "Invalid token: " value)))))

; Solve subproblem of ast-expr-node (an expression)
(define (parse-ast-expr-node lex)
  (let* ([left-child (parser lex)]
         [operator (parse-operator lex)]
         [right-child (parser lex)]
         [token (lex)]
         [type (token-type token)]
         [value (token-repr token)])
    (if (equal? type 'rparen)
        (ast-expr-node operator left-child right-child)
        (error "Syntax Error: Parenthesis mismatch"))))

(define (parse-operator lex)
  (let* ([tok (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
     (if (eq? typ 'op)
         val
         (error "Invalid Operation: " val))))
         


; ast -> val
; this function takes an AST and returns the calculated value
; note that we assume the tree was built correctly!
(define (eval ast)
   (match ast
     ([ast-node v] v)
     ([ast-expr-node operator left-child right-child] (if (eq? operator #\+)
                            (+ (eval left-child) (eval right-child))
                            (* (eval left-child) (eval right-child))))))

; str -> val
; takes a string, creates the lexer and parser and then evaluates it
(define (evalstr str)
  (let ([lexer (lexstr str)])
    (eval (parser lexer))))
