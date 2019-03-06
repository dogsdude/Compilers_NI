;Sam Lindsey, Project1, 1/24/2019

#lang racket
; needed for parsing stuff
(require parser-tools/lex
         
         ; this last gives us prettier names for common regular expression stuff,
         ; and also renames it so they're all prefixed with ':' in their names
         (prefix-in : parser-tools/lex-sre))

(require test-engine/racket-tests)

(define-tokens value-tokens (NUM ID STRING))
(define-empty-tokens paren-types (LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE))
(define-empty-tokens operators (ADD MULT DIV SUB DOT))
(define-empty-tokens punctuation (COMMA COLON SEMI))
(define-empty-tokens comparators (EQ NE LT GT LE GE))
(define-empty-tokens boolops (BOOLOR BOOLAND))

(define-empty-tokens keywords (AND ARRAY AS BREAK DEFINE DO ELSE END IF IN IS
 JUNIPER KIND LET NEEWOM NI NOW OF PENG THEN
 TO WHILE WITH))

(define-empty-tokens endoffile (EOF))

(define nilex
         (lexer-src-pos

          ;Values, Types, Operators, Punctuation, Comparators, Boolean Operations
          [#\(               (token-LPAREN)]
          [#\)               (token-RPAREN)]
          [#\[               (token-LBRACKET)]
          [#\]               (token-RBRACKET)]
          [#\{               (token-LBRACE)]
          [#\}               (token-RBRACE)]
          [#\+               (token-ADD)]
          [#\*               (token-MULT)]
          [#\/               (token-DIV)]
          [#\-               (token-SUB)]
          [#\.               (token-DOT)]
          [#\,               (token-COMMA)]
          [#\:               (token-COLON)]
          [#\;               (token-SEMI)]
          [#\=               (token-EQ)]
          [#\<               (token-LT)]
          ["<="              (token-LE)]
          [#\>               (token-GT)]
          [">="              (token-GE)]
          [#\|               (token-BOOLOR)]
          [#\&               (token-BOOLAND)]

          ;Keywords
          [(:or "AND" "And" "and")         (token-AND)]
          [(:or "ARRAY" "Array" "array")   (token-ARRAY)]
          [(:or "AS" "As" "as")            (token-AS)]
          [(:or "BREAK" "Break" "break")   (token-BREAK)]
          [(:or "DEFINE" "Define" "define")(token-DEFINE)]
          [(:or "DO" "Do" "do")            (token-DO)]
          [(:or "ELSE" "Else" "else")      (token-ELSE)]
          [(:or "END" "End" "end")         (token-END)]
          [(:or "IF" "If" "if")            (token-IF)]
          [(:or "IN" "In" "in")            (token-IN)]
          [(:or "IS" "Is" "is")            (token-IS)]
          [(:or "JUNIPER" "Juniper" "juniper") (token-JUNIPER)]
          [(:or "KIND" "Kind" "kind")      (token-KIND)]
          [(:or "LET" "Let" "let")         (token-LET)]
          [(:or "NEEWOM" "Neewom" "neewom")(token-NEEWOM)]
          [(:or "NI" "Ni" "ni")            (token-NI)]
          [(:or "NOW" "Now" "now")         (token-NOW)]
          [(:or "OF" "Of" "of")            (token-OF)]
          [(:or "PENG" "Peng" "peng")      (token-PENG)]
          [(:or "THEN" "Then" "then")      (token-THEN)]
          [(:or "TO" "To" "to")            (token-TO)]
          [(:or "WHILE" "While" "while")   (token-WHILE)]
          [(:or "WITH" "With" "with")      (token-WITH)]

          ;EOF
          [(eof) (token-EOF)]

          ;Num
          [(:+ numeric) (token-NUM lexeme)]
          
          ;Identifier
          [(:: (:+ alphabetic) (:* (:or numeric alphabetic #\_ #\-))(:* #\')) (token-ID lexeme)]
          
          ;String
          [(:: #\" (:* (:or (:: #\\ any-char)(complement (:or #\\ #\")))) #\") (token-STRING lexeme)]

          ;Single Line Comment
          [(:: "//" any-string #\newline) (return-without-pos (nilex input-port)) ]

          ;Block Comment
          [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (return-without-pos (nilex input-port))]

          ;White Space
          [whitespace (return-without-pos (nilex input-port))]
          
          ))

; position -> string -> error
; raises a lexing error
(define (raise-lex-error pos lexeme)
  (let* ([linenums? (not (eq? (position-line pos) #f))]
         [loc (if linenums? (position-line pos) (position-offset pos))]
         [col (position-col pos)]
         [partial-msg (string-append (if linenums? "syntax error at line "
                                         "syntax error at offset ") (number->string loc))]
         [msg (string-append partial-msg (if linenums? (string-append ", col " (number->string col)) "")
                             ": '" lexeme "'")])
    ;(lexer-error #t)
    (raise-syntax-error 'nilex msg)))


; input port -> thunk
; creates a thunk that when called will return the next token from the input stream
(define (get-tokenizer in)
  (λ () (nilex in)))


; input port -> list of tokens
; this function takes an input port and returns a list of
; tokens read from it (until it hits eof)
(define (lex in)
  (port-count-lines! in)
  (let ([tokenize (get-tokenizer in)])
    (define (lexfun)
      (let ([tok (tokenize)])
        (cond
          ; test to see if we hit eof as the base case
          [(eq? (position-token-token tok) (token-EOF)) null]
          [else (cons (position-token-token tok) (lexfun))])))
    (lexfun)))


; string -> list of tokens
; this function takes a string and returns a list of
; tokens read from it (until it reaches the end)
(define (lexstr str)
  (lex (open-input-string str)))

; filename -> list of tokens
; this function takes a filename, opens it as an input port,
; and then reads tokens until the end is reached
(define (lexfile filename)
  (lex (open-input-file filename)))

;Test Cases
(check-expect (lexstr "and") (list (token-AND)))
(check-expect (lexstr "array") (list (token-ARRAY)))
(check-expect (lexstr "as") (list (token-AS)))
(check-expect (lexstr "break") (list (token-BREAK)))
(check-expect (lexstr "do") (list (token-DO)))
(check-expect (lexstr "else") (list (token-ELSE)))
(check-expect (lexstr "end") (list (token-END)))
(check-expect (lexstr "if") (list (token-IF)))
(check-expect (lexstr "in") (list (token-IN)))
(check-expect (lexstr "is") (list (token-IS)))
(check-expect (lexstr "juniper") (list (token-JUNIPER)))
(check-expect (lexstr "kind") (list (token-KIND)))
(check-expect (lexstr "let") (list (token-LET)))
(check-expect (lexstr "neewom") (list (token-NEEWOM)))
(check-expect (lexstr "ni") (list (token-NI)))
(check-expect (lexstr "now") (list (token-NOW)))
(check-expect (lexstr "of") (list (token-OF)))
(check-expect (lexstr "peng") (list (token-PENG)))
(check-expect (lexstr "]") (list (token-RBRACKET)))
(check-expect (lexstr "[") (list (token-LBRACKET)))
(check-expect (lexstr "}") (list (token-RBRACE)))
(check-expect (lexstr "{") (list (token-LBRACE)))
(check-expect (lexstr ")") (list (token-RPAREN)))
(check-expect (lexstr "(") (list (token-LPAREN)))
(check-expect (lexstr "+") (list (token-ADD)))
(check-expect (lexstr "-") (list (token-SUB)))
(check-expect (lexstr "/") (list (token-DIV)))
(check-expect (lexstr "*") (list (token-MULT)))
(check-expect (lexstr ";") (list (token-SEMI)))
(check-expect (lexstr ",") (list (token-COMMA)))
(check-expect (lexstr ":") (list (token-COLON)))
(check-expect (lexstr ">") (list (token-GT)))
(check-expect (lexstr "<") (list (token-LT)))
(check-expect (lexstr "=") (list (token-EQ)))
(check-expect (lexstr ">=") (list (token-GE)))
(check-expect (lexstr "<=") (list (token-LE)))
(check-expect (lexstr "&") (list (token-BOOLAND)))
(check-expect (lexstr "|") (list (token-BOOLOR)))
(check-expect (lexstr "1") (list (token-NUM "1")))
(check-expect (lexstr "ax") (list (token-ID "ax")))
(check-expect (lexstr (string-append (string #\") (string #\\) (string #\") "Hello world" (string #\\) (string #\") (string #\")))
 (list (token-STRING (string-append (string #\") (string #\\) (string #\") "Hello world" (string #\\) (string #\") (string #\") ))))
(check-expect (lexstr "") '())
(check-expect (lexstr "ni") (list (token-NI)))
(check-expect (lexstr "5") (list (token-NUM "5")))
(check-expect (lexstr "56") (list (token-NUM "56")))
(check-expect (lexstr "/* Hello
there */") '())
(check-expect (lexstr "/* hello */5/*there*/") (list (token-NUM "5")))
(check-expect (lexstr "// this is a comment \n5+3") (list (token-NUM "5") (token-ADD) (token-NUM "3")))
(check-expect (lexstr "\"\\\\\"a\"\"") (list (token-STRING "\"\\\\\"") (token-ID "a") (token-STRING "\"\"")))
(check-expect (lexstr "\"you had me at \\\"hello\\\"\"") (list (token-STRING "\"you had me at \\\"hello\\\"\"")))          
          
(test)