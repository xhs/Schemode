#lang racket

;;; structs

(define-struct instruction (opcode args))

(define *last-type* #f)

(define-syntax asm
  (syntax-rules ()
    ((_ opcode arg+ ...)
     (let ((op
            (cond ((eq? 'mov opcode)
                   (cond ((eq? 'int *last-type*)
                          'movi)
                         ((eq? 'bool *last-type*)
                          'movb)
                         ((eq? 'char *last-type*)
                          'movc)))
                  (else
                   opcode))))
       (make-instruction op (list arg+ ...))))))

(define (repr x)
  (cond ((list? x)
         (map repr x))
        ((instruction? x)
         (cons (instruction-opcode x) (instruction-args x)))
        (else
         (error "invalid instruction:" x))))

(define-struct macro (id expander))

;;; utils

(define acc 'acc)
(define (sp offset) (list 'sp offset))

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

(define (variable-expr? e)
  (symbol? e))

(define (form-expr? e)
  (and (list? e)
       (not (null? e))))

(define (begin-expr? e)
  (eqv? 'begin (car e)))

(define (if-expr? e)
  (eqv? 'if (car e)))

(define (let-expr? e)
  (eqv? 'let (car e)))

(define (primitive-expr? e)
  (macro-lookup (car e) *primitive-macros*))

(define (macro-lookup id env)
  (cond ((null? env) #f)
        ((eq? (macro-id (car env)) id) (car env))
        (else (macro-lookup id (cdr env)))))

(define (lookup var env)
  (cond ((assv var env) => cdr)
        (else #f)))

;;; compiler

(define (compile e)
  (flatten (compile-expr e 1 '())))

(define (compile-expr e si env)
  (cond ((immediate-expr? e)
         (compile-immediate e))
        ((variable-expr? e)
         (compile-var e env))
        ((form-expr? e)
         (compile-form e si env))
        (else
         (error "unknown expression:" e))))

(define (compile-immediate e)
  (cond ((integer? e)
         (compile-integer e))
        ((boolean? e)
         (compile-bool e))
        ((char? e)
         (compile-char e))))

(define (compile-integer i)
  (set! *last-type* 'int)
  (asm 'mov i acc))

(define (compile-bool b)
  (set! *last-type* 'bool)
  (asm 'mov acc))

(define (compile-char c)
  (set! *last-type* 'char)
  (asm 'mov c acc))

(define (compile-var var env)
  (cond ((lookup var env) =>
         (lambda (si) (asm 'mov (sp si) acc)))
        (else
         (error "undefined variable:" var))))

(define (compile-form e si env)
  (cond ((begin-expr? e)
         (compile-begin e si env))
        ((if-expr? e)
         (compile-if e si env))
        ((let-expr? e)
         (compile-let e si env))
        (else
         (let ((m (macro-lookup (car e)
                                *primitive-macros*)))
           (if m
               ((macro-expander m) (cdr e) si env)
               (compile-proc e si env))))))

; wrong?
(define (compile-begin e si env)
  (map (lambda (x) (compile-expr x si env))
       (cdr e)))

(define (compile-if e si env)
  (let ((args (cdr e)))
    (if (= (length args) 3)
        (let ((alt (new-label 'L))
              (end (new-label 'L)))
          (list
           (compile-expr (car args) si env)
           (asm 'jmpf alt)
           (compile-expr (cadr args) si env)
           (asm 'jmp end)
           (asm 'label alt)
           (compile-expr (caddr args) si env)
           (asm 'label end)))
        (error "arity mismatch:" args))))

(define (compile-let e si env)
  (let loop ((bindings (cadr e))
             (si si)
             (env env))
    (if (empty? bindings)
        (compile-expr (caddr e) si env)
        (let ((b (car bindings)))
          (list (compile-expr (cadr b) si env)
                (asm 'mov acc (sp si))
                (loop (cdr bindings)
                      (+ 1 si)
                      (cons (cons (car b) si) env)))))))

(define (compile-proc e si env)
  (error "not implemented"))

(define *primitive-macros* '())

(define-syntax define-primitive
  (syntax-rules ()
    ((_ prim lam)
     (let ((m (make-macro 'prim lam)))
       (set! *primitive-macros* (cons m *primitive-macros*))))))

(define-primitive add1
  (lambda (args si env)
    (if (= (length args) 1)
        (list (compile-expr (car args) si env)
              (asm 'addi 1 acc))
        (error "arity mismatch:" args))))

(define-primitive +
  (lambda (args si env)
    (if (= (length args) 2)
        (let ((arg0 (car args))
              (arg1 (cadr args)))
          (if (integer? arg1)
              (list (compile-expr arg0 si env)
                    (asm 'addi arg1 acc))
              (if (integer? arg0)
                  (list (asm 'mov arg0 (sp si))
                        (compile-expr arg1 (+ 1 si) env)
                        (asm 'addi (sp si) acc))
                  (list (compile-expr arg0 si env)
                        (asm 'mov acc (sp si))
                        (compile-expr arg1 (+ 1 si) env)
                        (asm 'addi (sp si) acc)))))
        (error "arity mismatch:" args))))

(define-primitive =
  (lambda (args si env)
    (if (= (length args) 2)
        (let ((arg0 (car args))
              (arg1 (cadr args)))
          (if (integer? arg1)
              (list (compile-expr arg0 si env)
                    (asm 'mov arg1 (sp si))
                    (asm 'teq acc (sp si)))
              (list (compile-expr arg0 si env)
                    (asm 'mov acc (sp si))
                    (compile-expr arg1 (+ 1 si) env)
                    (asm 'teq acc (sp si)))))
        (error "arity mismatch:" args))))

;;; parser

(define (parse filename)
  (cons 'begin (file->list filename)))

;;; tests

(repr (compile (parse "test.scm")))
