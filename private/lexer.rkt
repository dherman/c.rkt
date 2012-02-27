#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/match
         "syntactic-context.rkt")

(provide Operators Separators EmptyLiterals Keywords BasicTokens
         str-tok StringErrors (struct-out string-error)
         c-lexer
         token-description)

;; XXX: most of this was lifted from the ProfessorJ parser; modify for C

;; =============================================================================
;; TOKENS
;; =============================================================================

;; TODO: Java -> C
(define-empty-tokens Operators
  (PIPE OR OREQUAL
   =    >       <       !       ~       ?       :
   ==   <=      >=      !=      &&      ++      --      ->
   +    -       *       /       &       ^       %       <<      >>      >>>
   +=   -=      *=      /=      &=      ^=      %=      <<=	>>=     >>>=))

(define (token-description name)
  (case name
    [(O_PAREN) "parenthesis (`(')"]
    [(C_PAREN) "parenthesis (`)')"]
    [(O_BRACE) "brace (`{')"]
    [(C_BRACE) "brace (`}')"]
    [(O_BRACKET) "bracket (`[')"]
    [(C_BRACKET) "bracket (`]')"]
    [(SEMI_COLON) "semi-colon (`;')"]
    [(COLON) "colon (`:')"]
    [(PERIOD) "period (`.')"]
    [(COMMA) "comma (`,')"]
    [(ELLIPSIS) "ellipsis (`...')"]
    [(EOF) "EOF"]
    [(auto break case char const continue default do double else enum extern float for
      goto if inline int long register restrict return short signed sizeof static struct
      switch typedef union unsigned void volatile while _Bool _Complex _Imaginary)
     (format "keyword `~a'" name)]
    [(UNSIGNED_LONG_INTEGER_LIT) "unsigned long integer literal"]
    [(LONG_INTEGER_LIT) "long integer literal"]
    [(UNSIGNED_LONG_LONG_INTEGER_LIT) "unsigned long long integer literal"]
    [(LONG_LONG_INTEGER_LIT) "long long integer literal"]
    [(UNSIGNED_INTEGER_LIT) "unsigned integer literal"]
    [(INTEGER_LIT) "integer literal"]
    [(FLOAT_LIT) "float literal"]
    [(DOUBLE_LIT) "double literal"]
    [(LONG_DOUBLE_LIT) "long double literal"]
    [(STRING_LIT) "string literal"]
    [(WSTRING_LIT) "wide string literal"]
    [(CHAR_LIT) "character literal"]
    [(WCHAR_LIT) "wide character literal"]
    [(IDENTIFIER) "identifier"]
    [(TYPEDEF_NAME) "type identifier"]
    [(STRING_ERROR) "bad or incomplete string literal"]
    [(NUMBER_ERROR) "bad or incomplete number literal"]
    [(READ_ERROR) "bad or incomplete read input"]
    [else (symbol->string name)]))

;; XXX: use L/R instead of O/C

(define-empty-tokens Separators
  (O_PAREN C_PAREN O_BRACE C_BRACE O_BRACKET C_BRACKET SEMI_COLON COLON PERIOD COMMA ELLIPSIS))

(define-empty-tokens EmptyLiterals
  (EOF))

(define-empty-tokens Keywords
  (auto break case char const continue default do double else
   enum extern float for goto if inline int long register
   restrict return short signed sizeof static struct switch typedef union
   unsigned void volatile while _Bool _Complex _Imaginary))

(define-tokens BasicTokens
  (UNSIGNED_LONG_INTEGER_LIT LONG_INTEGER_LIT
   UNSIGNED_LONG_LONG_INTEGER_LIT LONG_LONG_INTEGER_LIT 
   UNSIGNED_INTEGER_LIT INTEGER_LIT
   FLOAT_LIT DOUBLE_LIT LONG_DOUBLE_LIT
   STRING_LIT WSTRING_LIT CHAR_LIT WCHAR_LIT
   IDENTIFIER TYPEDEF_NAME STRING_ERROR NUMBER_ERROR READ_ERROR))

(define (trim-string s f l)
  (substring s f (- (string-length s) l)))

(define (integer-literal->token s base)
  (match (reverse (string->list (string-downcase s)))
    [(or (list #\u #\l #\l digits ...)
         (list #\l #\l #\u digits ...))
     (token-UNSIGNED_LONG_LONG_INTEGER_LIT (string->number (list->string (reverse digits)) base))]
    [(or (list #\u #\l digits ...)
         (list #\l #\u digits ...))
     (token-UNSIGNED_LONG_INTEGER_LIT (string->number (list->string (reverse digits)) base))]
    [(list #\l #\l digits ...)
     (token-LONG_LONG_INTEGER_LIT (string->number (list->string (reverse digits)) base))]
    [(list #\l digits ...)
     (token-LONG_INTEGER_LIT (string->number (list->string (reverse digits)) base))]
    [(list #\u digits ...)
     (token-UNSIGNED_INTEGER_LIT (string->number (list->string (reverse digits)) base))]
    [_ (token-INTEGER_LIT (string->number s base))]))

;(define (integer->token number suffix)
;  (if (not suffix)
;      number
;      (match (sort (string->list (string-downcase suffix)) char<?)
;        [(list #\l #\l #\u) (token-UNSIGNED_LONG_LONG_INTEGER_LIT number)]
;        [(list #\l #\u) (token-UNSIGNED_LONG_INTEGER_LIT number)]
;        [(list #\l #\l) (token-LONG_LONG_INTEGER_LIT number)]
;        [(list #\l) (token-LONG_INTEGER_LIT number)]
;        [(list #\u) (token-UNSIGNED_INTEGER_LIT number)])))

;; =============================================================================
;; REGULAR EXPRESSIONS
;; =============================================================================

(define-lex-abbrevs
  (CR #\015)
  (LF #\012)
  (LineTerminator (:or CR 
                       LF 
                       (:seq CR LF)))
  (InputCharacter (^ CR LF))

  (FF #\014)
  (TAB #\011)
  (WhiteSpace (:or #\space 
                   TAB
                   FF
                   LineTerminator))

  ;; (Had to transform CommentTail and CommentTailStar into one RE)
  (Comment (:or TraditionalComment 
                EndOfLineComment
                DocumentationComment))
  (TraditionalComment (:seq "/*" NotStar CommentTail))
  (EndOfLineComment (:seq "//" (:* (:~ CR LF))))
  (DocumentationComment (:seq "/**" CommentTailStar))
  (CommentTail (:seq (:* (:seq (:* NotStar) (:+ "*") NotStarNotSlash))
                     (:* NotStar)
                     (:+ "*")
                     "/"))
  (CommentTailStar (:seq (:* (:seq (:* "*") NotStarNotSlash (:* NotStar) "*"))
                         (:* "*")
                         "/"))
  (NotStar (:or (:~ "*")))
  (NotStarNotSlash (:or (:~ "*" "/")))

  (SyntaxComment (:or TraditionalCommentEOF
                      EndOfLineComment))
  (TraditionalCommentEOF (:seq "/*" CommentTailEOF))
  (CommentTailEOF (:or (:seq (:* (:seq (:* NotStar) (:+ "*") NotStarNotSlash))
                             (:* NotStar)
                             (:+ "*")
                             "/")
                       (:seq (:* (:seq (:* NotStar) (:+ "*") NotStarNotSlash))
                             (:* NotStar)
                             (:* "*"))))

  ;; (No need to worry about excluding keywords and such.  They will
  ;;  appear first in the lexer spec)
  (Identifier (:seq IDLetter (:* IDLetterOrDigit)))
  (IDLetter (:or (:/ "A" "Z")
                 (:/ "a" "z")
                 "_"
                 ;; XXX: UnivCharName
                 ))
  (UnivCharName (:or (:seq "\\u" HexQuad)
                     (:seq "\\U" HexQuad HexQuad)))
  (HexQuad (:seq HexDigit HexDigit HexDigit HexDigit))
  (IDLetterOrDigit (:or IDLetter
                        (:/ "0" "9")))

  (Keyword (:or "auto" "break" "case" "char" "const" "continue" "default" "do" "double" "else"
                "enum" "extern" "float" "for" "goto" "if" "inline" "int" "long" "register"
                "restrict" "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef" "union"
                "unsigned" "void" "volatile" "while" "_Bool" "_Complex" "_Imaginary"))

  (Digits (:+ (:/ #\0 #\9)))
  (DigitsOpt (:* (:/ #\0 #\9)))

  (UnsignedSuffix (:or "u" "U"))
  (LongSuffix (:or "l" "L"))
  (LongLongSuffix (:or "ll" "LL"))
  (IntegerTypeSuffix (:or (:seq UnsignedSuffix (:? LongSuffix))
                          (:seq UnsignedSuffix LongLongSuffix)
                          (:seq LongSuffix (:? UnsignedSuffix))
                          (:seq LongLongSuffix UnsignedSuffix)))
  (DecimalNumeral (:seq (:/ #\1 #\9) (:* (:/ #\0 #\9))))
  (HexDigit (:or (:/ #\0 #\9)
                 (:/ #\a #\f)
                 (:/ #\A #\F)))
  (HexNumeral (:or (:seq #\0 "x" (:+ HexDigit))
                   (:seq #\0 "X" (:+ HexDigit))))
  (OctalNumeral (:seq #\0 (:* (:/ #\0 #\7))))

  (FloatTypeSuffix (:or "f" "F"))
  (LongDoubleTypeSuffix (:or "l" "L"))

  (FloatA (:seq Digits #\. DigitsOpt (:? ExponentPart)))
  (FloatB (:seq #\. Digits (:? ExponentPart)))
  (FloatC (:seq Digits ExponentPart))

  (ExponentPart (:seq (:or "e" "E") (:? (:or "+" "-")) Digits))

  ;; XXX: hex escapes
  (EscapeSequence (:or "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v" "\\'" "\\\"" "\\?" "\\\\"
                       (:seq #\\ (:/ #\0 #\3) (:/ #\0 #\7) (:/ #\0 #\7))
                       (:seq #\\ (:/ #\0 #\7) (:/ #\0 #\7))
                       (:seq #\\ (:/ #\0 #\7))))

  (Operator (:or "->"
                 "++" "--" "&" "*" "+" "-" "~" "!"
                 "/" "%" "<<" ">>" "<" ">" "<=" ">=" "==" "!=" "^" "|" "&&" "||"
                 "?"
                 "=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|=")))

;  (Operator (:or    	">" "<" "!"	"~"	"?"	":"
;                 "=="	"<="	">="	"!="	"&&" "||"	"++"	"--"	"->"
;                 "+"	"-"	"*"	"/"	"&" "|"	"^"	"%"	"<<" ">>" ">>>"
;                 "+="	"-="	"*="	"/="	"&="	"|="	"^="	"%="	"<<="	">>="	">>>=")))

;; String tokens
(define-tokens str-tok (STRING_CHAR STRING_ESCAPE))
(define-empty-tokens StringErrors (STRING_END STRING_EOF STRING_NEWLINE))

(define-struct string-error (string error-token) #:transparent)

;; =============================================================================
;; LEXERS
;; =============================================================================

;; tokens->string : (listof position-token) -> string
(define (tokens->string toks)
  (list->string
   (reverse
    (for/fold ([out null])
              ([in (map (compose token-value position-token-token) toks)])
      (if (char? in)
          (cons in out)
          (append (reverse (string->list in)) out))))))
;  (list->string (map (compose token-value position-token-token) toks)))

;; string-lexer : position input-port -> position-token
(define (string-lexer token first-token-pos in)
  (let* ([tokens (get-string-tokens in)]
         [rev-tokens (reverse tokens)]
         [last-token (car rev-tokens)]
         [str (tokens->string (reverse (cdr rev-tokens)))])
    (position-token
     (if (eq? 'STRING_END (get-token-name last-token))
         (token str)
         (token-STRING_ERROR (string-error str (position-token-token last-token))))
     first-token-pos
     (position-token-end-pos last-token))))

;; get-string-tokens : input-port -> (listof position-token)
(define (get-string-tokens in)
  (let ((tok (get-string-token in)))
    (case (get-token-name tok)
      ((STRING_EOF STRING_END STRING_NEWLINE) (list tok))
      (else (cons tok (get-string-tokens in))))))

;; get-string-token : input-port -> position-token
(define get-string-token
  (lexer-src-pos
   (#\" (token-STRING_END))
   (EscapeSequence (token-STRING_ESCAPE lexeme)); (EscapeSequence->char lexeme)))
   ((:~ CR LF) (token-STRING_CHAR (string-ref lexeme 0)))
   ((:or CR LF) (token-STRING_NEWLINE))
   (#\032 (token-STRING_EOF))
   ((eof) (token-STRING_EOF))))

;; get-token-name : position-token -> symbol
(define (get-token-name tok)
  (token-name (position-token-token tok)))

(define (EscapeSequence->char es)
  (match es
    ["\\a" #\007]
    ["\\b" #\010]
    ["\\f" #\014]
    ["\\n" #\012]
    ["\\r" #\015]
    ["\\t" #\011]
    ["\\v" #\013]
    ["\\?" #\?]
    ["\\'" #\']
    ["\\\"" #\"]
    ["\\\\" #\\]
    ;; XXX: hex escapes
    [else (integer->char (string->number (trim-string es 1 0) 8))]))

(define (c-lexer ps ls)

  (define (return x)
    (begin (save-token! ls x) x))

  (lexer-src-pos
   ["=" (begin
          (when (declarator-context? ps)
            (pop-lexer-declarator! ls ps "="))
          (return '=))]
   [Operator (return
              (case lexeme
                [("|") (token-PIPE)]
                [("||") (token-OR)]
                [("|=") (token-OREQUAL)]
                [else (string->symbol lexeme)]))]

   ["(" (begin
          (parenthesis++ ls)
          (return (token-O_PAREN)))]
   [")" (begin
          (parenthesis-- ls)
          (return (token-C_PAREN)))]
   [(:or "{" "<%") (begin
                     (brace++ ls)
                     (return (token-O_BRACE)))]
   [(:or "}" "%>") (begin
                     (brace-- ls)
                     (if (and (lexer-state-read? ls)
                              (zero? (lexer-state-brace-depth ls)))
                         (return 'EOF)
                         (return (token-C_BRACE))))]
   [(:or "[" "<:") (return (token-O_BRACKET))]
   [(:or "]" ":>") (return (token-C_BRACKET))]
   [":" (return (token-COLON))]
   [";" (begin
          (when (declarator-context? ps)
            (pop-lexer-declarator! ls ps ";"))
          (return (token-SEMI_COLON)))]
   ["," (begin
          (when (declarator-context? ps)
            (pop-lexer-declarator! ls ps ","))
          (return (token-COMMA)))]
   ["..." (return (token-ELLIPSIS))]
   ["." (return (token-PERIOD))]

   [(:seq #\L #\") (return-without-pos (return (string-lexer token-WSTRING_LIT start-pos input-port)))]
   [#\" (return-without-pos (return (string-lexer token-STRING_LIT start-pos input-port)))]

   [(:seq #\L #\' (:~ CR LF #\\) #\')
    (return (token-WCHAR_LIT (string-ref lexeme 2)))]
   [(:seq #\L #\' EscapeSequence #\')
    (return (token-WCHAR_LIT (trim-string lexeme 2 1)))] ;(EscapeSequence->char (trim-string lexeme 2 1))))]
   [(:seq #\' (:~ CR LF #\' #\\) #\')
    (return (token-CHAR_LIT (trim-string lexeme 1 1)))]
   [(:seq #\' EscapeSequence #\') 
    (return (token-CHAR_LIT (trim-string lexeme 1 1)))] ;(EscapeSequence->char (trim-string lexeme 1 1))))]

   [(:or FloatA FloatB FloatC)
    (return (token-DOUBLE_LIT (string->number lexeme)))]
   [(:seq (:or FloatA FloatB FloatC) FloatTypeSuffix)
    (return (token-FLOAT_LIT (string->number (trim-string lexeme 0 1))))]
   [(:seq (:or FloatA FloatB FloatC) LongDoubleTypeSuffix)
    (return (token-LONG_DOUBLE_LIT (string->number (trim-string lexeme 0 1))))]

   ;; XXX: HexadecimalFloatingConstant

   [DecimalNumeral
    (return (integer-literal->token lexeme 10))]
   [(:seq DecimalNumeral IntegerTypeSuffix)
    (return (integer-literal->token lexeme 10))]
   [HexNumeral
    (return (integer-literal->token (substring lexeme 2) 16))]
   [(:seq HexNumeral IntegerTypeSuffix)
    (return (integer-literal->token (substring lexeme 2) 16))]
   [OctalNumeral
    (return (integer-literal->token lexeme 8))]
   [(:seq OctalNumeral IntegerTypeSuffix)
    (return (integer-literal->token lexeme 8))]

   [Keyword (return (string->symbol lexeme))]

   [Identifier (let ([id (string->symbol lexeme)])
                 (debug `(C lexer identifier ,id))
                 (debug (marshall-state ps ls))
                 (return
                  (cond
                    [(lookup id (parser-state-env ps))
                     => (lambda (binding)
                          (if (eq? binding 'type)
                              (begin (debug `(C lexer identifier ,id TYPEDEF_NAME))
                                     (token-TYPEDEF_NAME id))
                              (begin (debug `(C lexer identifier ,id IDENTIFIER))
                                     (token-IDENTIFIER id))))]
                    [else (begin (debug `(C lexer identifier ,id IDENTIFIER))
                                 (token-IDENTIFIER id))])))]

   [Comment (return-without-pos ((c-lexer ps ls) input-port))]

   [(:+ WhiteSpace) (return-without-pos ((c-lexer ps ls) input-port))]

   [#\032 (if (and (lexer-state-read? ls)
                   (> (lexer-state-brace-depth ls) 0))
              (return (token-READ_ERROR "expected a `}' to close `{'"))
              (return 'EOF))]
   [(eof) (if (and (lexer-state-read? ls)
                   (> (lexer-state-brace-depth ls) 0))
              (return (token-READ_ERROR "expected a `}' to close `{'"))
              (return 'EOF))]

   [(:+ (:or (:/ #\0 #\9)(:/ #\a #\z)(:/ #\A #\Z)))
    (return (token-NUMBER_ERROR lexeme))]
   ))
