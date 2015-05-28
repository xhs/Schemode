#lang racket

(require racket/match)
(require racket/pretty)

;; utilities

(define new-label
  (let ((count 0))
    (lambda (x)
      (set! count (add1 count))
      (string->symbol
       (string-append (symbol->string x)
                      "_"
                      (number->string count))))))

;; environment

(define-struct binding (id val))

(define (extend id val set)
  (cons (make-binding id val) set))

(define (bulk-extend ids vals set)
  (append (map make-binding ids vals) set))

(define (lookup id set)
  (cond ((null? set) #f)
        ((eq? (binding-id (car set)) id)
         (binding-val (car set)))
        (else (lookup id (cdr set)))))

;; primitive

(define-struct macro (id emitter))

(define *primitives* '())

(define-syntax define-primitive
  (syntax-rules ()
    ((_ prim emitter)
     (set! *primitives*
           (cons (make-macro (quote prim) emitter)
                 *primitives*)))))

(define (macro-lookup sym macros)
  (cond ((null? macros) #f)
        ((eq? (macro-id (car macros)) sym)
         (macro-emitter (car macros)))
        (else (macro-lookup sym (cdr macros)))))

(define (primitive? sym)
  (macro-lookup sym *primitives*))

(define-primitive + #t)

;; parse

(define (parse filename)
  (cons 'begin (file->list filename)))

;; alpha conversion

(define *global-vars* '())

(define (var-lookup id env)
  (or (lookup id env)
      (lookup id *global-vars*)))

(define (special? sym)
  (or (member sym '(begin))
      (primitive? sym)))

(define (alpha-convert expr)
  (define (convert expr env)
    (match expr
      ((? symbol?)
       (or (var-lookup expr env)
           (error 'alpha-convert (format "undefined symbol: ~a" expr))))
      (`(set! ,(and id (? symbol?)) ,val)
       `(set! ,(or (var-lookup id env)
                   (let ((g (new-label id)))
                     (set! *global-vars*
                           (cons (make-binding id g)
                                 *global-vars*))
                     g))
              ,(convert val env)))
      (`(define ,(and id (? symbol?)) ,val)
       (convert `(set! ,id ,val) env))
      (`(define (,(and fn (? symbol?)) ,args ...) ,body ...)
       (convert `(set! ,fn
                       (lambda (,@args)
                         ,@body))
                env))
      (`(lambda (,formals ...) ,body ...)
       (let ((new-env (bulk-extend formals
                                   (map new-label formals)
                                   env)))
         `(lambda (,@(map (lambda (v) (var-lookup v new-env))
                          formals))
            ,(convert (cons 'begin body) new-env))))
      (`(let (,bindings ...) ,body ...)
       (let* ((ids (map car bindings))
              (new-env (bulk-extend ids
                                    (map new-label ids)
                                    env)))
         `(let (,@(map (lambda (v)
                         `(,(var-lookup (car v) new-env)
                           ,(convert (cadr v) new-env)))
                       bindings))
            ,(convert (cons 'begin body) new-env))))
      (`(,(and special (? special?)) ,expr* ...)
       `(,special ,@(map (lambda (v) (convert v env)) expr*)))
      (`(,expr+ ...)
       `(,@(map (lambda (v) (convert v env)) expr+)))
      (else expr)))
  (convert expr '()))

;; compile

(define (pipe input . pass*)
  (let loop ((input input)
             (pass* pass*))
    (if (null? pass*)
        input
        (loop ((car pass*) input)
              (cdr pass*)))))

(define (compile filename)
  (pipe filename
        parse
        alpha-convert
        pretty-print))

;; test

(compile "test.scm")
