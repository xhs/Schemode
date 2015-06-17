#lang racket

(require parser-tools/lex)
(require parser-tools/lex-sre)
(require ragg/support)
(require "grammer.rkt")

(define (string->boolean s)
  (match s
    ("true" #t)
    ("false" #f)))

(define tokenize
  (lexer
   ((: "//" (* (- any-char #\newline)) #\newline)
    (token 'COMMENT lexeme #:skip? #t))
   ; boolean
   ((or "true" "false")
    (token 'BOOLEAN (string->boolean lexeme)))
   ; keyword
   ((or "->" "=" ":=" "end")
    lexeme)
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
    lexeme)
   ; delimeter
   ((or #\( #\))
    lexeme)
   ; skip
   ((+ (or blank #\return #\newline))
    (token 'WHITESPACE lexeme #:skip? #t))
   ((eof)
    (void))))

(define in (open-input-string "
c = 0

count = () ->
  // c is free variable
  c := c + 1
  c
end

count()
"))

(define (scan port)
  (let loop ((t (tokenize port))
             (acc '()))
    (if (void? t)
        (reverse acc)
        (loop (tokenize port) (cons t acc)))))

(syntax->datum (parse (scan in)))
