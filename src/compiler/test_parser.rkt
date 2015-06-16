#lang racket

(require parser-tools/lex)
(require parser-tools/lex-sre)
(require ragg/support)
(require "parser.rkt")

(define (string->boolean s)
  (match s
    ("true" #t)
    ("false" #f)))

(define tokenize
  (lexer
   ((or "true" "false")
    (token 'BOOLEAN (string->boolean lexeme)))
   ("->"
    (token 'ARROW lexeme))
   ("recur"
    (token 'RECUR lexeme))
   ; identifier
   ((: (or alphabetic #\$ #\_)
       (* (or alphabetic numeric #\$ #\_)))
    (token 'IDENTIFIER lexeme))
   ; integer
   ((: (? (or #\+ #\-))
       (+ numeric))
    (token 'INTEGER (string->number lexeme)))
   ; primitive
   ((or #\+ #\- #\* #\/)
    (token 'PRIMITIVE lexeme))
   (#\=
    (token 'ASSIGN lexeme))
   (#\(
    (token 'LEFT-PAREN lexeme))
   (#\)
    (token 'RIGHT-PAREN lexeme))
   ((+ (or blank #\return #\newline))
    (token 'WHITESPACE lexeme #:skip? #t))
   ((eof)
    (void))))

(define in (open-input-string "add = (a b) -> a + b"))
(tokenize in)
