#lang racket

(require racket/pretty)

;;;

(define-struct ast (children))

(define-struct (literal ast) (val))
(define-struct (reference ast) (var))
(define-struct (assignment ast) (var))
(define-struct (conditional ast) ())
(define-struct (primitive ast) (op))
(define-struct (application ast) ())
(define-struct (&lambda ast) (params))
(define-struct (&sequence ast) ())

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
                        (make-&sequence (expand-exprs (cdr expr) env))))))
   (make-macro 'let
               (lambda (expr env)
                 (expand-expr (cons (cons 'lambda
                                          (cons (map car (cadr expr))
                                                (cddr expr)))
                                    (map cadr (cadr expr)))
                              env)))))

(define (parse-file filename)
  (set! *global-env* (make-initial-env))
  (expand-expr (cons 'begin (file->list filename))
               '()))

;;;

(define (diff x y)
  (cond ((null? x) '())
        ((memq (car x) y) (diff (cdr x) y))
        (else (cons (car x) (diff (cdr x) y)))))

(define (union x y)
  (cond ((null? x) y)
        ((memq (car x) y) (union (cdr x) y))
        (else (cons (car x) (union (cdr x) y)))))

(define (free-variables ast)
  (cond ((reference? ast)
         (list (reference-var ast)))
        ((assignment? ast)
         (union (free-variables (car (ast-children ast)))
                (list (assignment-var ast))))
        ((&lambda? ast)
         (diff (free-variables (car (ast-children ast)))
               (&lambda-params ast)))
        (else
         (foldl union '() (map free-variables (ast-children ast))))))

(define (cps-transform ast)
  (define (cps ast cont)
    (cond ((literal? ast)
           (make-application (list cont ast)))
          ((reference? ast)
           (make-application (list cont ast)))
          ((assignment? ast)
           (cps-list (ast-children ast)
                     (lambda (x)
                       (make-application
                        (list cont
                              (make-assignment x (assignment-var ast)))))))
          ((conditional? ast)
           (let ((transform
                  (lambda (x)
                    (cps-list (list (car (ast-children ast)))
                              (lambda (test)
                                (make-conditional
                                 (list (car test)
                                       (cps (cadr (ast-children ast))
                                            x)
                                       (cps (caddr (ast-children ast))
                                            x))))))))
             (if (reference? cont)
                 (transform cont)
                 (let ((k (new-variable 'k)))
                   (make-application
                    (list (make-&lambda
                           (list (transform (make-reference '() k)))
                           (list k))
                          cont))))))
          ((primitive? ast)
           (cps-list (ast-children ast)
                     (lambda (args)
                       (make-application
                        (list cont
                              (make-primitive args
                                              (primitive-op ast)))))))
          ((application? ast)
           (let ((f (car (ast-children ast))))
             (if (&lambda? f)
                 (cps-list (cdr (ast-children ast))
                           (lambda (vals)
                             (make-application
                              (cons (make-&lambda
                                     (list (cps-seq (ast-children f) cont))
                                     (&lambda-params f))
                                    vals))))
                 (cps-list (ast-children ast)
                           (lambda (args)
                             (make-application
                              (cons (car args)
                                    (cons cont (cdr args)))))))))
          ((&lambda? ast)
           (let ((k (new-variable 'k)))
             (make-application
              (list cont
                    (make-&lambda
                     (list (cps-seq (ast-children ast)
                                    (make-reference '() k)))
                     (cons k (&lambda-params ast)))))))
          ((&sequence? ast)
           (cps-seq (ast-children ast) cont))
          (else
           (error "unknown ast: ~a~n" ast))))
  (define (cps-list asts inner)
    (define (body x)
      (cps-list (cdr asts)
                (lambda (new-asts)
                  (inner (cons x new-asts)))))
    (cond ((null? asts)
           (inner '()))
          ((or (literal? (car asts))
               (reference? (car asts)))
           (body (car asts)))
          (else
           (let ((r (new-variable 'r)))
             (cps (car asts)
                  (make-&lambda
                   (list (body (make-reference '() r)))
                   (list r)))))))
  (define (cps-seq asts cont)
    (cond ((null? asts)
           (make-application (list cont #f)))
          ((null? (cdr asts))
           (cps (car asts) cont))
          (else
           (let ((r (new-variable 'r)))
             (cps (car asts)
                  (make-&lambda
                   (list (cps-seq (cdr asts) cont))
                   (list r)))))))
  (let ((cps-ast
         (cps ast
              (let ((r (new-variable 'r)))
                (make-&lambda
                 (list (make-primitive (list (make-reference '() r))
                                       '%halt))
                 (list r))))))
    (if (lookup 'call/cc (free-variables ast))
        (make-application
         (list (make-&lambda
                (list cps-ast)
                (list (new-variable '_)))
               (expand-expr '(set! call/cc
                                   (lambda (k f)
                                     (f k (lambda (_ result) (k result)))))
                            '())))
        cps-ast)))

;;;

(define (keep f l)
  (cond ((null? l) '())
        ((f (car l)) (cons (car l) (keep f (cdr l))))
        (else (keep f (cdr l)))))

(define (index-in-list x l)
  (let loop ((l l)
             (i 0))
    (cond ((not (pair? l)) #f)
          ((eq? (car l) x) i)
          (else
           (loop (cdr l) (+ 1 i))))))

(define (closure-transform ast)
  (define (convert ast self free-vars)
    (define (convert1 ast)
      (cond ((literal? ast)
             ast)
            ((reference? ast)
             (let ((i (index-in-list (reference-var ast)
                                     free-vars)))
               (if i
                   (make-primitive
                    (list (make-reference '() self)
                          (make-literal '() (+ 1 i)))
                    '%vector-ref)
                   ast)))
            ((assignment? ast)
             (make-assignment (map convert1 (ast-children ast))
                              (assignment-var ast)))
            ((conditional? ast)
             (make-conditional (map convert1 (ast-children ast))))
            ((primitive? ast)
             (make-primitive (map convert1 (ast-children ast))
                             (primitive-op ast)))
            ((application? ast)
             (let ((fn (car (ast-children ast)))
                   (args (map convert1 (cdr (ast-children ast)))))
               (if (&lambda? fn)
                   (make-application
                    (cons (make-&lambda
                           (list (convert1 (car (ast-children fn))))
                           (&lambda-params fn))
                          args))
                   (let ((f (convert1 fn)))
                     (make-application
                      (cons (make-primitive
                             (list f (make-literal '() 0))
                             '%vector-ref)
                            (cons f args)))))))
            ((&lambda? ast)
             (let ((new-free-vars
                    (keep (lambda (v)
                            (not (global-variable? v)))
                          (free-variables ast)))
                   (new-self (new-variable 'self)))
               (make-primitive
                (cons (make-&lambda
                       (list (convert (car (ast-children ast))
                                      new-self
                                      new-free-vars))
                       (cons new-self
                             (&lambda-params ast)))
                      (map (lambda (x)
                             (convert1 (make-reference '() x)))
                           new-free-vars))
                '%vector)))
            ((&sequence? ast)
             (make-&sequence (map convert1 (ast-children ast))))
            (else
             (error "unknown ast: ~a~n" ast))))
    (convert1 ast))
  (make-&lambda (list (convert ast #f '())) '()))

;;;



;;;

(define *debug* #t)

(define (repr ast)
  (cond ((literal? ast)
         (literal-val ast))
        ((reference? ast)
         (variable-uid (reference-var ast)))
        ((assignment? ast)
         (list 'set!
               (variable-uid (assignment-var ast))
               (repr (car (ast-children ast)))))
        ((conditional? ast)
         (cons 'if (map repr (ast-children ast))))
        ((primitive? ast)
         (cons (primitive-op ast) (map repr (ast-children ast))))
        ((application? ast)
         (if (&lambda? (car (ast-children ast)))
             (list 'let
                   (map (lambda (x y)
                          (list (variable-uid x) (repr y)))
                        (&lambda-params (car (ast-children ast)))
                        (cdr (ast-children ast)))
                   (repr (car (ast-children (car (ast-children ast))))))
             (map repr (ast-children ast))))
        ((&lambda? ast)
         (list 'lambda
               (map variable-uid (&lambda-params ast))
               (repr (car (ast-children ast)))))
        ((&sequence? ast)
         (cons 'begin (map repr (ast-children ast))))
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

(define (inspect ast)
  (pretty-print (repr ast)))

;;;

(define a (parse-file "test.scm"))
(inspect a)

(define cp (cps-transform a))
(inspect cp)

(define cl (closure-transform cp))
(inspect cl)
