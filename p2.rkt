#lang racket
; needed for parsing stuff
(require parser-tools/yacc
         
         ; this last gives us prettier names for common regular expression stuff,
         ; and also renames it so they're all prefixed with ':' in their names
         (prefix-in lex: parser-tools/lex)
         "project1.rkt")

(require test-engine/racket-tests)




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
(struct Bool (val) #:transparent)
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

(define niparser
  (parser
   (src-pos)
   (start expression)
   (end EOF)
   (tokens value-tokens paren-types operators punctuation comparators boolops keywords endoffile)
   ; Implement Error Method so we can tell where the parser fails
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                        (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?))))
   (grammar
    
    ;;expressions, there are many of them

     ;Used in our type declaration, a type-id must be an ID
    ;Used here because it is referred as a type-id frequentally in the documentation
    (type-id
     [(ID)      $1])

    ; ;Typefields are used in our type declarations so we define them here
;     ;TODO:: I think the pattern matching here is wrong... RECORD AND ARRAY EXPR?
;     (typefields
;      [(type-id ID LPAREN COMMA type-id id RPAREN) 
; 
;     ;Again, we use this here to pattern match on certain cases of a ty in the type declaration
;     (ty
;      [(type-id)  $1]
;      [(LBRACE typefields RBRACE)  $2]
;      [(ARRAY OF type-id) $3


    ;;;Is everything an expression or do they each need own section?
    (expression

     ;Variable Declarations
     [(NI type-id ID IS expression) (VarDecl $2 $3 $5)]
     ;This may need to be changed!
     [(NI ID IS expression) (VarDecl $2 #f $4)]

     ;Integer Declaration
     [(NUM) (NumExpr $1)]

     ;String Declaration
     [(STRING) (StringExpr $1)]

     ;Boolean Declarations
     [(BOOL)  (Bool $1)]

     ;Type Declaration... How to pull from arraytype or int,bool,str, or record...? What is this next thing?
     ;[(DEFINE ID KIND AS ty) (]

     ; ;Function Declarations... What is this next thing? Mutual recursion? What?
;      FunDecl (name args rettype body next)
;      [(NEEWOM ID LPAREN typefields RPAREN IS expression) (FunDecl $2  
;      [(NEEWOM ID LPAREN typefields RPAREN AS type-id IS expression) (FunDecl $2


     ;Scope Rules... Do we need these??

     ; ;LValues
;       (lvalue
;        [(ID) $1]
;        [(lvalue DOT ID) (RecordExpr $1 $2)]
;        [(lvalue LBRACKET expression RBRACKET) (ArrayExpr $1 $3)]
;        )


     ;Valueless Expression

     ;peng Expression
     ;[(PENG

     ;Sequencing Expressions
     ;[(LPAREN expression SEMI expression)*]

     ;No Value
     [(LPAREN RPAREN)    (NoVal)]
     ;A let expression with nothing between in and end does not yield a value.
     ;[(LET 

     ;Negation
     ;[(SUB NUM)

     ;Function Call

     ;Arithmetic

     ;Boolean Comparison

     ;String Comparison
    
     ))))