#lang racket

;;; utils

(define new-label
  (let ((count 0))
    (lambda (id)
      (set! count (add1 count))
      (string->symbol
       (string-append (symbol->string id)
                      "_"
                      (number->string count))))))

(define (tag-list? tag ls)
  (eq? tag
       (and (list? ls)
            (not (null? ls))
            (car ls))))

(define (keyword? x)
  (or (member x '(quote begin let lambda)) ; if cond let* letrec letrec* set! define apply call/cc
      (primitive? x)))

;;; env

(define (check-id x)
  (if (variable? x)
      x
      (error 'check-id (format "invalid variable: ~a" x))))

(define (make-binding lhs rhs)
  (check-id lhs)
  (list lhs rhs))
(define binding-id car)
(define binding-val cadr)
(define variable? symbol?)

(define (make-initial-env) '())

(define (extend-env id val env)
  (cons (make-binding id val) env))

(define (bulk-extend-env ids vals env)
  (append (map make-binding ids vals) env))

(define (lookup id env)
  (cond ((assv id env) => binding-val)
        (else #f)))

(define (lhs b)
  (check-id (car b)))

(define rhs cadr)

;;; quote

(define (quote? expr) (tag-list? 'quote expr))

;;; begin

(define (begin? expr) (tag-list? 'begin expr))
(define (make-begin ls)
  (if (null? (cdr ls))
      (car ls)
      (cons 'begin ls)))

;;; let

(define (make-let bindings body)
  (list 'let bindings body))
(define let-bindings cadr)
(define (let-body expr)
  (make-begin (cddr expr)))
(define (let? expr) (tag-list? 'let expr))

;;; lambda

(define (make-lambda formals body)
  (list 'lambda formals body))
(define lambda-formals cadr)
(define (lambda-body expr)
  (make-begin (cddr expr)))
(define (lambda? expr) (tag-list? 'lambda expr))

;;; primitive

(define *primitive-macros* '())

(define-syntax define-primitive
  (syntax-rules ()
    ((_ prim expander)
     (let ((m (make-macro (quote prim) expander)))
       (set! *primitive-macros*
             (cons m *primitive-macros*))))))

(define make-macro cons)
(define macro-prim car)
(define macro-expander cdr)
(define (primitive? x) (macro-lookup x *primitive-macros*))

(define (macro-lookup prim macros)
  (cond ((null? macros) #f)
        ((eq? (macro-prim (car macros)) prim) (car macros))
        (else (macro-lookup prim (cdr macros)))))

(define-primitive + #f)

;;; α-conversion
;;; λx.x => λy.y

(define (alpha-conversion expr)
  (define (convert expr env)
    (cond ((variable? expr)
           (or (lookup expr env)
               (error 'α-conversion (format "undefined variable: ~a" expr))))
          ((lambda? expr)
           (let* ((formals (lambda-formals expr))
                  (new-env
                   (bulk-extend-env formals
                                    (map new-label formals)
                                    env)))
             (make-lambda (map (lambda (x) (lookup x new-env)) formals)
                          (convert (lambda-body expr) new-env))))
          ((quote? expr) expr)
          ((let? expr)
           (let* ((bindings (let-bindings expr))
                  (ids (map lhs bindings))
                  (new-env
                   (bulk-extend-env ids
                                    (map new-label ids)
                                    env)))
             (make-let (map (lambda (b)
                              (make-binding (lookup (lhs b) new-env)
                                            (convert (rhs b) env)))
                            bindings)
                       (convert (let-body expr) new-env))))
          ((and (list? expr) (not (null? expr)) (keyword? (car expr)))
           (cons (car expr) (map (lambda (e) (convert e env)) (cdr expr))))
          ((list? expr)
           (map (lambda (e) (convert e env)) expr))
          (else expr)))
  (convert expr (make-initial-env)))

;;; parser

(define (parse filename)
  (cons 'begin (file->list filename)))

;;; test

(alpha-conversion (parse "test.scm"))
