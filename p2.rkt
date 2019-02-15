#lang racket

;;;Sam Lindsey, 2/8/2019, Parser Project for Ni

; needed for parsing stuff
(require parser-tools/yacc
         
         ; this last gives us prettier names for common regular expression stuff,
         ; and also renames it so they're all prefixed with ':' in their names
         (prefix-in lex: parser-tools/lex)
         "project1.rkt")

(require test-engine/racket-tests)


(provide (all-defined-out))

;;;STRUCTS TO STORE OUR INFO IN

; var declarations
(struct VarDecl (type id expr) #:transparent)

; type declarations--note they can be mutually recursive (using AND)
; so our struct has a link to the next one that belongs here, otherwise
; it's simply '()
(struct NameType (name kind next) #:transparent)
(struct RecordType (name fields next) #:transparent)
(struct ArrayType (name kind next) #:transparent)
(struct TypeField (name kind) #:transparent)

; defines a function in ni
; these consist of the name of the function, the arguments to it,
; the return type (which may be #f if it doesn't have one) and the body
; finally, next points to the next, related definition (for mutual recursion)
(struct FunDecl (name args rettype body next) #:transparent)

; things associated with expressions and lvalues
(struct NumExpr (val) #:transparent)
; variable expressions
(struct VarExpr (name) #:transparent)
;bool (just the type)
(struct BoolVal (val) #:transparent)
; record expressions (name and a list of fields are required)
(struct RecordExpr (name field) #:transparent)
; array expressions (name and expression for the index)
(struct ArrayExpr (name expr) #:transparent)
; function call which is name and a list of arguments
(struct FuncallExpr (name args) #:transparent)
; a string
(struct StringExpr (str) #:transparent)
; a noval 
(struct NoVal () #:transparent)
; a list of declarations for the let and a list of expressions following it
(struct LetExpr (decs exprs) #:transparent)
; arithmetic expression
(struct MathExpr (expr1 op expr2) #:transparent)
; bool op, i.e., comparision
(struct BoolExpr (expr1 op expr2) #:transparent)
; logic op, and or or
(struct LogicExpr (expr1 op expr2) #:transparent)
; assignment in a field for creating a record
(struct FieldAssign (name expr) #:transparent)
; creating a new record
(struct NewRecordExpr (name assignments) #:transparent)
; creating a new array
(struct NewArrayExpr (name expr kind) #:transparent)
; an if expression (hint, you may temporarily use an IfElseExpr if you
; would like to make it easy to see when you're matching or not
(struct IfExpr (test true-branch false-branch) #:transparent)
; a while expression, which is a test and the body
(struct WhileExpr (test body) #:transparent)
; an assignment expression
(struct AssignmentExpr (name expr) #:transparent)
; break expression--this has no arguments
(struct BreakExpr () #:transparent)
; peng expression -- no arguments
(struct PengExpr () #:transparent)
; with expression (think: for expression)
(struct WithExpr (idname initexpr fromexpr toexpr) #:transparent)



; input port -> ni ast   
; construct an ast from the input port
(define (build-ast in)
  (port-count-lines! in)
  (niparser (get-tokenizer in)))

; string representing ni code -> ni ast
; parses a string and turns it into an ast if possible
(define (parse-str str)
  (let ([in (open-input-string str)])
    (build-ast in)))

; string (filename) -> ni ast
; opens and parses a file and tries to turn it into an ast if possible
(define (parse-file filename)
  (let ([in (open-input-file filename)])
    (build-ast in)))

;The Parser itself
(define niparser
  (parser
   (src-pos)
   (start program)
   (end EOF)
   (tokens value-tokens paren-types operators punctuation comparators boolops keywords endoffile)
   ; Implement Error Method so we can tell where the parser fails
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                        (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?))))
   
   ;Everything used in our grammar is found below
   (grammar

    ;This is used to creat our list of expressions
    (program
     [(expression) (list $1)])

    ;Type Declarations
    (tydec
     
     ;Non-recursive
     ;Name
     [(type-id KIND AS type-id) (NameType $1 $4 '())]
     ;Records
     [(type-id KIND AS LBRACE typefields RBRACE) (RecordType $1 $5 '())]
     ;Arrays
     [(type-id KIND AS ARRAY OF type-id) (ArrayType $1 $6 '())]
     
     ;Recursive
     ;Mutually Recursive, it could be any of these things called again or the same one -> definition of mutual vs normal recursion
     ;Name
     [(type-id KIND AS type-id AND DEFINE tydec)   (NameType $1 $4 $7)]
     ;Records
     [(type-id KIND AS LBRACE typefields RBRACE AND DEFINE tydec) (RecordType $1 $5 $9)]
     ;Arrays
     [(type-id KIND AS ARRAY OF type-id AND DEFINE tydec)  (ArrayType $1 $6 $9)])
  
    ;Typefields
    ;Are used in our type declarations so we define them here
    (typefields
     [()  ('())]
     [(type-id ID COMMA typefields)    (cons (TypeField $2 $1) $4)]
     [(type-id ID)  (cons (TypeField $2 $1) '())])
     
    ;Type-field 
    ;Used in our type declaration, a type-id must be an ID
    ;Used here because it is referred as a type-id frequentally in the documentation
    (type-id
     [(ID)      $1])

    ;Variable Declarations
    (varbdec
     [(type-id ID IS expression) (VarDecl $1 $2 $4)]
     [(ID IS expression)         (VarDecl #f $1 $3)])

    ;Function Declarations
    (func
     
     ;Non-recursive
     ;Procedure = function that does not return a value
     [(ID LPAREN typefields RPAREN IS expression) (FunDecl $1 $3 #f $6 '())]
     [(ID LPAREN RPAREN IS expression) (FunDecl $1 '() #f $5 '())]
     ;Function that returns a value                                                         
     [(ID LPAREN typefields RPAREN AS type-id IS expression) (FunDecl $1 $3 $6 $8 '())]
     ;;BETTER WAY TO IMPLEMENT AN EMPTY TYPEFIELDS?
     [(ID LPAREN RPAREN AS type-id IS expression) (FunDecl $1 '() $5 $7 '())]

     
     ;Recursive
     ;Procedure
     [(ID LPAREN typefields RPAREN IS expression AND NEEWOM func)   (FunDecl $1 $3 #f $6 $9)]
     [(ID LPAREN RPAREN IS expression AND NEEWOM func)   (FunDecl $1 '() #f $5 $8)]
     ;Function that returns a value
     [(ID LPAREN typefields RPAREN AS type-id IS expression AND NEEWOM func) (FunDecl $1 $3 $6 $8 $11)]
     [(ID LPAREN RPAREN AS type-id IS expression AND NEEWOM func) (FunDecl $1 '() $5 $7 $10)])

    ;    ;LValues
    ;    (lvalue
    ;     ;Variable
    ;     [(ID) $1]
    ;     ;Record
    ;     [(ID DOT ID) (RecordExpr $1 $3)]
    ;     ;Array
    ;     [(ID LBRACKET expression RBRACKET) (ArrayExpr $1 $3)]
    ;     )

    ;    ;Sequencing... HOW TO EVALUATE EACH EXPRESSION?
    ;    (seq
    ;   
    ;     [(expression RPAREN)    (cons ($1) '())]
    ;     [(expression SEMI seq)  (cons ($1) $3)])

    ;

    ;Record Creation
    (record
     [(ID IS expression RBRACE) (cons (FieldAssign $1 $3) '())]
     [(ID IS expression COMMA record) (cons (FieldAssign $1 $3) $5)])
    
     
    ;OUR BUNCHES AND BUNCHES OF EXPRESSIONS!
    (expression
     
     ;Type Declaration
     [(DEFINE tydec)        $2]
     
     ;Variable Declarations
     [(NI varbdec)               $2]

     ;Integer Declaration
     [(NUM) (NumExpr $1)]

     ;String Declaration
     [(STRING) (StringExpr $1)]

     ;Boolean Declarations
     [(BOOL)  (BoolVal $1)]

     ;Function Declarations
     [(NEEWOM func)       $2]

     ;LValues
     ;[(lvalue)  $1]

     ;Valueless Expression... I assumed this to be the same thing as NoVal

     ;Peng Expression, essentially a NULL
     [(PENG)   (PengExpr '())]

     ;Sequencing Expressions
     ;[(LPAREN expression SEMI seq)  $4]

     ;No Value
     [(LPAREN RPAREN)    (NoVal)]
     
     ;A let expression with nothing between in and end does not yield a value.
     ;[(LET 

     ;Negation
     [(SUB NUM) (MathExpr (NumExpr "0") '- (NumExpr $2))]

     ;Function Call - 1st is normal call, 2nd is empty call
     [(ID LPAREN funcall)  (FuncallExpr $1 $3)]
     [(ID LPAREN RPAREN)   (FuncallExpr $1 '())]

     ;Arithmetic
     ;(arithmetic
     [(expression ADD expression)    (MathExpr  $1 '+ $3)]
     [(expression SUB expression)    (MathExpr  $1 '- $3)]
     [(expression DIV expression)    (MathExpr  $1 '/ $3)]
     [(expression MULT expression)   (MathExpr  $1 '* $3)]
     

     ;Boolean Comparison, String Comp is parsed the same way as Boolean Comp so we don't need additional steps here
     ;(BoolComp
     [(expression EQ expression)     (BoolExpr $1 'eq $3)]
     [(expression LT expression)     (BoolExpr $1 'lt $3)]
     [(expression LE expression)     (BoolExpr $1 'le $3)]
     [(expression GE expression)     (BoolExpr $1 'ge $3)]
     [(expression GT expression)     (BoolExpr $1 'gt $3)]
             
       
     ;Logic Operators, sub expression must be boolean expressions
     ;(LogOp
     [(expression BOOLOR expression)   (LogicExpr $1 'or  $3)]
     [(expression BOOLAND expression)  (LogicExpr $1 'and $3)]
     
     ;Presedence of Operators, Negate, ( * , / ) ( + , - ) ( & , | ) ( = , <= , <> , >= , > )
     ;How to represent this?
     ;[(

     ;Associativity of operators

     ;Record Creation
     ;type-id '{'id is expr (, id is expr)*'}' or type-id '{' '}', for an empty expression, creates a new record of type type-id
     [(type-id LBRACE record) (NewRecordExpr $1 $3)]
     [(type-id LBRACE RBRACE) (NewRecordExpr $1 '())]

     ;Array Creation
     ;The expression type-id '[' expr ']' of expr2 evaluations expr and expr2 (in that order) to find the number of elements and the initial value
     [(type-id LBRACKET expression RBRACKET OF expression)    (NewArrayExpr $1 $3 $6)]

     ;Array & Record Assignment

     ;Extent
     
     ;Assignment

     ;if-then-else
     ;(conditionals
     [(IF expression THEN expression ELSE expression END)    (IfExpr $2 $4 $6)]

     ;if-then, looks the same as an if-then-else, but with no false-branch -> put the empty list instead
     [(IF expression THEN expression END) (IfExpr $2 $4 '())]  

     ;while
     [(WHILE expression DO expression END) (WhileExpr $2 $4)]
     
     ;with, id - initexpr - fromexpr - toexpr
     [(WITH ID AS expression TO expression DO expression END) (WithExpr $2 $4 $6 $8)]
     
     ;Break
     [(BREAK)    (BreakExpr '())]
     
     ;let
     ;let decs in exprseq end
     ;[(LET 

     ;Parentheses
     ;[(LPAREN expression RPAREN)    
    
     ))))


;;;TEST CASES

; var declarations
(check-expect (parse-str "ni x is 5") (list (VarDecl #f "x" (NumExpr "5"))))
; type declarations
(check-expect (parse-str "define int2 kind as int") (list (NameType "int2" "int" '())))
(check-expect (parse-str "define intarr kind as array of int") (list (ArrayType "intarr" "int" '())))
(check-expect (parse-str "define intrec kind as { int x }")
              (list (RecordType "intrec" (list (TypeField "x" "int")) '())))
; function declarations
(check-expect (parse-str "neewom getX() as int is 5")
              (list (FunDecl "getX" '() "int" (NumExpr "5") '())))
; function calls of various sorts
(check-expect (parse-str "add2(5)") (list (FuncallExpr "add2" (list (NumExpr "5")))))
; parens
(check-expect (parse-str "(5)") (list (NumExpr "5")))
; various sequences
(check-expect (parse-str "(6; 5)") (list (list (NumExpr "6") (NumExpr "5"))))
; strings
(check-expect (parse-str "\"Hello World\"") (list (StringExpr "\"Hello World\"")))
; noval
(check-expect (parse-str "()") (list (NoVal)))
; let expressions
(check-expect (parse-str "let ni x is 5 in x end")
              (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (VarExpr "x")))))
; math ops
(check-expect (parse-str "1+2")
              (list (MathExpr (NumExpr "1") '+ (NumExpr "2"))))
; math ops using negated numbers
(check-expect (parse-str "-5") (list (MathExpr (NumExpr "0") '- (NumExpr "5"))))

; bool expressions
(check-expect (parse-str "5=6") (list (BoolExpr (NumExpr "5") 'eq (NumExpr "6"))))

; array creation
(check-expect (parse-str "intarr[10] of 6")
              (list (NewArrayExpr "intarr" (NumExpr "10") (NumExpr "6"))))

; record expression
(check-expect (parse-str "point { x is 6 }")
              (list (NewRecordExpr "point" (list (FieldAssign "x" (NumExpr "6"))))))

(test)