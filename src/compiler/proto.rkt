#lang racket

(require racket/pretty)

;;;

(define-struct ast (expr))

(define-struct (literal ast) (val))
(define-struct (reference ast) (var))
(define-struct (assignment ast) (var))
(define-struct (conditional ast) ())
(define-struct (primitive ast) (op))
(define-struct (application ast) ())
(define-struct (&lambda ast) (params))
(define-struct (&sequence ast) ())

;;;

(define *debug* #f)

(define (repr ast)
  (cond ((literal? ast)
         (literal-val ast))
        ((reference? ast)
         (variable-uid (reference-var ast)))
        ((assignment? ast)
         (list 'set!
               (variable-uid (assignment-var ast))
               (repr (car (ast-expr ast)))))
        ((conditional? ast)
         (cons 'if (map repr (ast-expr ast))))
        ((primitive? ast)
         (cons (primitive-op ast) (map repr (ast-expr ast))))
        ((application? ast)
         (if (&lambda? (car (ast-expr ast)))
             (list 'let
                   (map (lambda (x y)
                          (list (variable-uid x) (repr y)))
                        (&lambda-params (car (ast-expr ast)))
                        (cdr (ast-expr ast)))
                   (repr (car (ast-expr (car (ast-expr ast))))))
             (map repr (ast-expr ast))))
        ((&lambda? ast)
         (list 'lambda
               (map variable-uid (&lambda-params ast))
               (repr (car (ast-expr ast)))))
        ((&sequence? ast)
         (cons 'begin (map repr (ast-expr ast))))
        (else
         (error "unknown ast: ~a~n" ast))))

(define (binding-repr b)
  (cond ((variable? b)
         (list '%V (binding-id b) (variable-uid b)))
        ((macro? b)
         (list '%M (binding-id b)))
        (else
         (error "unknown binding: ~a~n" b))))

(define (env-repr env)
  (let loop ((env env)
             (acc '()))
    (if (null? env)
        (reverse acc)
        (loop (cdr env) (cons (binding-repr (car env)) acc)))))

;;;

(define-struct binding (id))

(define-struct (variable binding) (uid))
(define-struct (macro binding) (expander))

(define (extend bindings env)
  (append bindings env))

(define (lookup id env)
  (cond ((null? env) #f)
        ((eq? (binding-id (car env)) id) (car env))
        (else (lookup id (cdr env)))))

;;;

(define *variable-sequence* 0)

(define (new-variable id)
  (set! *variable-sequence* (+ 1 *variable-sequence*))
  (make-variable
   id
   (string->symbol
    (string-append (symbol->string id)
                   "."
                   (number->string *variable-sequence*)))))

(define (new-global-variable id)
  (make-variable id id))

(define (global-variable? var)
  (eq? (binding-id var) (variable-uid var)))

;;;

(define (expand-expr expr env)
  (if *debug*
      (printf "[expand-expr] expr: ~a, env: ~a~n" expr (env-repr env))
      #f)
  (cond ((constant? expr) (expand-constant expr))
        ((identifier? expr) (expand-identifier expr env))
        ((form? expr) (expand-form expr env))
        (else (error "invalid expression: ~a~n" expr))))

(define (constant? expr) (or (boolean? expr) (number? expr)))
(define (identifier? expr) (symbol? expr))
(define (form? expr) (and (not (null? expr)) (list? expr)))

(define (expand-constant const)
  (make-literal '() const))

(define (expand-identifier identifier env)
  (let ((x (expand-lookup identifier env)))
    (if (variable? x)
        (make-reference '() x)
        (error "reference non-variable: ~a~n" identifier))))

(define (expand-form form env)
  (let* ((x (car form))
         (y (and (identifier? x) (expand-lookup x env))))
    (if (macro? y)
        ((macro-expander y) form env)
        (make-application (expand-exprs form env)))))

(define (expand-exprs exprs env)
  (map (lambda (e) (expand-expr e env)) exprs))

(define *global-env* '())

(define (expand-lookup id env)
  (if *debug*
      (printf "[expand-lookup] id: ~a, env: ~a~n" id (env-repr (append env *global-env*)))
      #f)
  (or (lookup id env)
      (lookup id *global-env*)
      (let ((v (new-global-variable id)))
        (set! *global-env* (cons v *global-env*))
        v)))

(define (make-initial-env)
  (list
   (make-macro '+
               (lambda (expr env)
                 (make-primitive (expand-exprs (cdr expr) env) '%+)))
   (make-macro '-
               (lambda (expr env)
                 (make-primitive (expand-exprs (cdr expr) env) '%-)))
   (make-macro '*
               (lambda (expr env)
                 (make-primitive (expand-exprs (cdr expr) env) '%*)))
   (make-macro '/
               (lambda (expr env)
                 (make-primitive (expand-exprs (cdr expr) env) '%/)))
   (make-macro 'set!
               (lambda (expr env)
                 (let ((x (expand-lookup (cadr expr) '())))
                   (if (variable? x)
                       (make-assignment (expand-exprs (cddr expr) env) x)
                       (error "assign non-variable: ~a~n" expr)))))
   (make-macro 'define
               (lambda (expr env)
                 (expand-expr (cons 'set! (cdr expr)) env)))
   (make-macro 'if
               (lambda (expr env)
                 (cond ((= (length (cdr expr)) 3)
                        (make-conditional (expand-exprs (cdr expr) env)))
                       ((= (length (cdr expr)) 2)
                        (expand-expr `(if ,(cadr expr) ,(caddr expr) #f) env)))))
   (make-macro 'lambda
               (lambda (expr env)
                 (let* ((params (map new-variable (cadr expr)))
                        (new-env (extend params env)))
                   (make-&lambda (list (expand-expr (cons 'begin (cddr expr)) new-env))
                                 params))))
   (make-macro 'begin
               (lambda (expr env)
                 (cond ((= (length (cdr expr)) 0)
                        (expand-expr #f env))
                       ((= (length (cdr expr)) 1)
                        (expand-expr (cadr expr) env))
                       (else
                        (make-&sequence (expand-exprs (cdr expr) env))))))))

(define (parse-file filename)
  (set! *global-env* (make-initial-env))
  (expand-expr (cons 'begin (file->list filename))
               '()))

;;;

(pretty-print (repr (parse-file "test.scm")))
