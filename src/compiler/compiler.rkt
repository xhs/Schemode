#lang racket

;;; structs

(define-struct inst (op))
(define-struct (sinst inst) (arg))

(define (repr x)
  (cond ((list? x)
         (map repr x))
        ((sinst? x)
         (list (inst-op x) (sinst-arg x)))
        ((inst? x)
         (list (inst-op x)))
        (else
         (error "invalid instruction:" x))))

(define-struct macro (prim expander))

;;; utils

(define new-label
  (let ((count 0))
    (lambda (id)
      (set! count (add1 count))
      (string->symbol
       (string-append (symbol->string id)
                      "."
                      (number->string count))))))

(define (immediate-expr? e)
  (or (integer? e) (boolean? e) (char? e)))

(define (form-expr? e)
  (and (list? e)
       (not (null? e))))

(define (begin-expr? e)
  (eqv? 'begin (car e)))

(define (if-expr? e)
  (eqv? 'if (car e)))

(define (prim-expr? e)
  (macro-lookup (car e) *primitive-macros*))

;;; compiler

(define (compile e)
  (flatten (compile-expr e)))

(define (compile-expr e)
  (cond ((immediate-expr? e)
         (compile-immediate e))
        ((form-expr? e)
         (compile-form e))
        (else
         (error "unknown expression:" e))))

(define (compile-immediate e)
  (cond ((integer? e)
         (compile-integer e))
        ((boolean? e)
         (compile-bool e))
        ((char? e)
         (compile-char e))
        (else
         (error "invalid immediate:" e))))

(define (compile-integer i)
  (make-sinst '%pushi i))

(define (compile-bool b)
  (make-sinst '%pushb b))

(define (compile-char c)
  (make-sinst '%pushc c))

(define (compile-form e)
  (cond ((begin-expr? e)
         (compile-begin e))
        ((if-expr? e)
         (compile-if e))
        (else
         (let ((m (macro-lookup (car e)
                                *primitive-macros*)))
           (if m
               ((macro-expander m) (cdr e))
               (compile-proc e))))))

(define (compile-begin e)
  (map compile-expr (cdr e)))

(define (compile-if e)
  (let ((args (cdr e)))
    (if (= (length args) 3)
        (let ((alt-label (new-label 'L))
              (end-label (new-label 'L)))
          (list
           (compile-expr (car args))
           (make-sinst '%jmpf alt-label)
           (compile-expr (cadr args))
           (make-sinst '%jmp end-label)
           (make-sinst '%label alt-label)
           (compile-expr (car (cddr args)))
           (make-sinst '%label end-label)))
        (error "invalid if statement:" args))))

(define (compile-proc e)
  '())

(define *primitive-macros*
  (list
   (make-macro 'add1
               (lambda (args)
                 (if (= (length args) 1)
                     (list (compile-expr (car args))
                           (make-sinst '%pushi 1)
                           (make-inst '%addi))
                     (error "arity mismatch:" args))))
   (make-macro '+
               (lambda (args)
                 (if (>= (length args) 2)
                     (list
                      (compile-expr (car args))
                      (let loop ((args (cdr args))
                                 (acc '()))
                        (if (null? args)
                            (reverse acc)
                            (loop (cdr args)
                                  (cons (list
                                         (compile-expr (car args))
                                         (make-inst '%addi))
                                        acc)))))
                     (error "arity mismatch:" args))))))

(define (macro-lookup x env)
  (cond ((null? env) #f)
        ((eq? (macro-prim (car env)) x) (car env))
        (else (macro-lookup x (cdr env)))))

;;; parser

(define (parse filename)
  (cons 'begin (file->list filename)))

;;; tests

(define asm (compile (parse "test.scm")))

(repr asm)
