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
                      "."
                      (number->string count))))))

(define (special? sym)
  (or (member sym '(begin apply if))
      (primitive? sym)))

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

(define *global-vars* '())

(define (var-lookup id env)
  (or (lookup id env)
      (lookup id *global-vars*)))

(define (global-var? id)
  (define (lookup id env)
    (cond ((null? env) #f)
          ((eq? (binding-val (car env)) id) #t)
          (else (lookup id (cdr env)))))
  (lookup id *global-vars*))

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
(define-primitive - #t)
(define-primitive * #t)
(define-primitive vector #t)
(define-primitive vector-ref #t)
(define-primitive set! #t)

;; parse

(define (parse filename)
  (cons 'begin (file->list filename)))

;; alpha conversion

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
      (`(let* (,bindings ...) ,body ...)
       (let ((head (car bindings))
             (tail (cdr bindings)))
         (if (null? tail)
             (convert `(let (,head)
                         ,@body)
                      env)
             (convert `(let (,head)
                         (let* (,@tail)
                           ,@body))
                      env))))
      (`(letrec ((,ids ,vals) ...) ,body ...)
       (let ((temps (map (lambda (_) (new-label 't))
                         ids)))
         (convert `(let (,@(map (lambda (id)
                                  `(,id #f))
                                ids))
                     (let (,@(map (lambda (temp val)
                                    `(,temp ,val))
                                  temps vals))
                       ,@(map (lambda (id temp)
                                `(set! ,id ,temp))
                              ids temps))
                     ,@body)
                  env)))
      (`(,(and special (? special?)) ,expr* ...)
       `(,special ,@(map (lambda (v) (convert v env)) expr*)))
      (`(,expr+ ...)
       `(,@(map (lambda (v) (convert v env)) expr+)))
      (else expr)))
  (convert expr '()))

;; cps conversion

(define (atomic? expr)
  (match expr
    (`(lambda (,_ ...) ,_) #t)
    ((or (? symbol?)
         (? integer?)
         (? boolean?)
         (? char?)) #t)
    (else #f)))

(define (M expr)
  (match expr
    (`(lambda (,formals ...) ,body)
     (let (($cont (new-label 'cont)))
       `(lambda (,@formals ,$cont)
          ,(T-c body $cont))))
    ('call/cc
     `(lambda (k f)
        (f k (lambda (_ result) (k result)))))
    ((? atomic?) expr)
    (else
     (error 'M (format "invalid expression: ~a" expr)))))

(define (T-c expr cont)
  (match expr
    ((? atomic?)
     `(,cont ,(M expr)))
    (`(begin ,expr)
     (T-c expr cont))
    (`(begin ,expr ,expr+ ...)
     (T-k expr (lambda (_)
                 (T-c `(begin ,@expr+) cont))))
    (`(if ,test ,conseq ,altern)
     (let (($cont (new-label 'cont)))
       `((lambda (,$cont)
           ,(T-k test (lambda ($test)
                        `(if ,$test
                             ,(T-c conseq $cont)
                             ,(T-c altern $cont)))))
         $cont)))
    (`(let ((,ids ,vals) ...) ,body)
     (T*-k vals (lambda ($vals)
                  `(let (,@(map list ids $vals))
                     ,(T-c body cont)))))
    (`(,(and prim (? primitive?)) ,args ...)
     (T*-k args (lambda ($prim)
                  `(,cont (,prim ,@args)))))
    (`(,fn ,args ...)
     (T-k fn (lambda ($fn)
               (T*-k args (lambda ($args)
                            `(,$fn ,@$args ,cont))))))))

(define (T-k expr k)
  (match expr
    ((? atomic?)
     (k (M expr)))
    (`(begin ,expr)
     (T-k expr k))
    (`(begin ,expr ,expr+ ...)
     (T-k expr (lambda (_)
                 (T-k `(begin ,@expr+) k))))
    (`(if ,test ,conseq ,altern)
     (let* ((r (new-label 'r))
            (cont `(lambda (,r) ,(k r))))
       (T-k test (lambda ($test)
                   `(if ,$test
                        ,(T-c conseq cont)
                        ,(T-c altern cont))))))
    (`(let ((,ids ,vals) ...) ,body)
     (T*-k vals (lambda ($vals)
                  `(let (,@(map list ids $vals))
                     ,(T-k body k)))))
    (`(,_ ,_ ...)
     (let* ((r (new-label 'r))
            (cont `(lambda (,r) ,(k r))))
       (T-c expr cont)))))

(define (T*-k exprs k)
  (if (null? exprs)
      (k '())
      (T-k (car exprs) (lambda (head)
                      (T*-k (cdr exprs) (lambda (tail)
                                       (k (cons head tail))))))))

(define (cps-convert expr)
  (T*-k expr (lambda (x) x)))

;; closure conversion

(define keep filter)

(define (diff s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (diff (cdr s1) s2))
        (else (cons (car s1) (diff (cdr s1) s2)))))

(define (union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (union (cdr s1) s2))
        (else (cons (car s1) (union (cdr s1) s2)))))

(define (union-multi sets) (foldl union '() sets))

(define (free-variables expr)
  (match expr
    ((or (? integer?)
         (? boolean?)
         (? char?)
         (? special?))
     '())
    ((? symbol?) (list expr))
    (`(lambda (,formals ...) ,body)
     (diff (free-variables body) formals))
    (`(let ((,ids ,vals) ...) ,body)
     (append (union-multi (map free-variables vals))
             (diff (free-variables body) ids)))
    (else
     (union-multi (map free-variables expr)))))

(define (index-of x lst)
  (let loop ((lst lst) (index 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) index)
          (else (loop (cdr lst) (add1 index))))))

(define (closure-convert expr)
  (define (convert expr closure fvs)
    (define (convert1 expr)
      (match expr
        ((or (? integer?)
             (? boolean?)
             (? char?))
         expr)
        ((? symbol?)
         (let ((index (index-of expr fvs)))
           (if index
               `(vector-ref ,closure ,index)
               expr)))
        (`(let ((,ids ,vals) ...) ,body)
         `(let (,@(map list ids (map convert1 vals)))
            ,(convert1 body)))
        (`(lambda (,formals ...) ,body)
         (let ((new-fvs (keep (lambda (x)
                                (not (global-var? x)))
                              (free-variables expr))))
           (if (null? new-fvs)
               `(lambda (,@formals) ,(convert1 body))
               (let ((new-closure (new-label 'closure)))
                 `(let ((,new-closure
                         (vector ,@(map convert1 new-fvs))))
                    (lambda (,@formals)
                      ,(convert body new-closure new-fvs)))))))
        (`(,(and prim (? primitive?)) ,args ...)
         `(,prim ,@(map convert1 args)))
        (`(,(and special (? special?)) ,expr+ ...)
         `(,special ,@(map convert1 expr+)))
        ((? list?)
         (map convert1 expr))
        (else
         (error 'closure-convert (format "unknown expression: ~a" expr)))))
    (convert1 expr))
  (convert expr #f '()))

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
        cps-convert
        closure-convert
        pretty-print))

;; test

(compile "test.scm")
