#lang racket

(require racket/match)
(require racket/pretty)

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
  (or (member x '(quote begin let if lambda)) ; cond let* letrec letrec* set! define apply call/cc
      (primitive? x)))

(define (merge-exprs x y)
  (cond ((and (begin? x) (begin? y))
         (make-begin (append (begin-seq x) (begin-seq y))))
        ((begin? x)
         (make-begin (append (begin-seq x) (list y))))
        ((begin? y)
         (make-begin (cons x (begin-seq y))))
        (else
         (make-begin (list x y)))))

(define (pipe input . pass*)
  (let loop ((input input)
             (pass* pass*))
    (if (null? pass*)
        input
        (loop ((car pass*) input)
              (cdr pass*)))))

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

;;; immediate

(define (immediate? x)
  (or (integer? x) (boolean? x) (null? x) (char? x)))

;;; quote

(define quote-body cadr)
(define (quote? expr) (tag-list? 'quote expr))

;;; begin

(define begin-seq cdr)
(define (begin? expr) (tag-list? 'begin expr))
(define (make-begin ls)
  (if (null? (cdr ls))
      (car ls)
      (cons 'begin ls)))

;;; if

(define if-test cadr)
(define if-conseq caddr)
(define if-altern cadddr)
(define (if? expr) (tag-list? 'if expr))

;;; let

(define (make-let bindings body)
  `(let ,bindings ,body))
(define let-bindings cadr)
(define (let-body expr)
  (make-begin (cddr expr)))
(define (let? expr) (tag-list? 'let expr))

;;; lambda

(define (make-lambda formals body)
  `(lambda ,formals ,body))
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
(define-primitive %constant #f)
(define-primitive %constant-ref #f)
(define-primitive %halt #f)

;;; application

(define (app? expr)
  (and (list? expr) (not (null? expr))))
(define (apply? expr) (tag-list? 'apply expr))
(define (app-fn expr)
  (if (apply? expr)
      (cadr expr)
      (car expr)))
(define (app-args expr)
  (if (apply? expr)
      (cddr expr)
      (cdr expr)))

;;; α conversion
;;; makes all variables unique

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

;;; lift constants
;;; merges redundant constants

(define (merge-constants expr)
  (let ((constants '()))
    (define (convert expr)
      (cond ((quote? expr)
             (cond ((immediate? (quote-body expr))
                    (quote-body expr))
                   ((assoc expr constants) => cadr)
                   (else
                    (let* ((const (new-label 'const))
                           (inst `(%constant-ref ,const)))
                      (set! constants
                            (cons (list expr inst)
                                  constants))
                      inst))))
            ((string? expr)
             (convert `(quote ,expr)))
            ((list? expr)
             (map convert expr))
            (else expr)))
    (let ((res (convert expr)))
      (if (null? constants)
          res
          (merge-exprs (make-begin
                        (map (lambda (x)
                               `(%constant ,(cadadr x)
                                           ,(quote-body (car x))))
                             constants))
                       res)))))

;;; cps conversion
;;; desugars continuations into functions

(define (atomic? expr)
  (or (lambda? expr)
      (immediate? expr)
      (string? expr)
      (symbol? expr)))

(define (M e)
  (match e
    [`(lambda (,formals ...) ,body)
     (let (($cont (new-label 'cont)))
       `(lambda (,@formals ,$cont)
          ,(T-c body $cont)))]
    ['call/cc
     `(lambda (k f)
        (f k (lambda (_ result) (k result))))]
    [(? atomic?) e]
    [else
     (error 'M (format "invalid expression: ~a" e))]))

(define (T-c e cont)
  (match e
    [(? atomic?)
     `(,cont ,(M e))]
    [`(begin ,expr)
     (T-c expr cont)]
    [`(begin ,expr ,expr+ ...)
     (T-k expr (lambda (_)
                 (T-c `(begin ,@expr+) cont)))]
    [`(if ,test ,conseq ,altern)
     (let (($cont (new-label 'cont)))
       `((lambda (,$cont)
           ,(T-k test (lambda ($test)
                        `(if ,$test
                             ,(T-c conseq $cont)
                             ,(T-c altern $cont)))))
         $cont))]
    [`(let ((,ids ,vals) ...) ,body)
     `(let (,@(map make-binding ids (map M vals)))
        ,(T-c body cont))]
    [`(,(and prim (? primitive?)) ,args ...)
     (T*-k args (lambda ($prim)
                  `(,cont (,prim ,@args))))]
    [`(,fn ,args ...)
     (T-k fn (lambda ($fn)
               (T*-k args (lambda ($args)
                            `(,$fn ,@$args ,cont)))))]))

(define (cps fn)
  (lambda (args ... k)
    (k (apply fn args ...))))

(define (T-k e k)
  (match e
    [(? atomic?)
     (k (M e))]
    [`(begin ,expr)
     (T-k expr k)]
    [`(begin ,expr ,expr+ ...)
     (T-k expr (lambda (_)
                 (T-k `(begin ,@expr+) k)))]
    [`(if ,test ,conseq ,altern)
     (let* ((r (new-label 'r))
            (cont `(lambda (,r) ,(k r))))
       (T-k test (lambda ($test)
                   `(if ,$test
                        ,(T-c conseq cont)
                        ,(T-c altern cont)))))]
    [`(let ((,ids ,vals) ...) ,body)
     `(let (,@(map make-binding ids (map M vals)))
        ,(T-k body k))]
    [`(,_ ,_ ...)
     (let* ((r (new-label 'r))
            (cont `(lambda (,r) ,(k r))))
       (T-c e cont))]))

(define (T*-k es k)
  (if (null? es)
      (k '())
      (T-k (car es) (lambda (hd)
                      (T*-k (cdr es) (lambda (tl)
                                       (k (cons hd tl))))))))

(define (cps-conversion expr)
  (T*-k expr (lambda (x) x)))

;;; parser

(define (parse filename)
  (cons 'begin (file->list filename)))

;;; compiler

(define (compile filename)
  (pipe filename
        parse
        alpha-conversion
        cps-conversion
        merge-constants
        pretty-print))

;;; test

(compile "test.scm")
