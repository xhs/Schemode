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
(define-struct (variable binding) (refnum) #:mutable)

(define (extend id val set)
  (cons (make-binding id val) set))

(define (bulk-extend ids vals set)
  (append (map (lambda (id val)
                 (make-variable id val 0))
               ids vals)
          set))

(define (lookup id set)
  (cond ((null? set) #f)
        ((eq? (binding-id (car set)) id)
         (car set))
        (else (lookup id (cdr set)))))

(define *global-vars* '())

(define (var-lookup id env)
  (let ((b 
         (or (lookup id env)
             (lookup id *global-vars*))))
    (and b
         (begin
           (set-variable-refnum! b (add1 (variable-refnum b)))
           (binding-val b)))))

(define (global-var? id)
  (define (lookup id env)
    (cond ((null? env) #f)
          ((eq? (binding-val (car env)) id)
           (car env))
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

(define (macro-lookup sym)
  (let loop ((macros *primitives*))
    (cond ((null? macros) #f)
          ((eq? (macro-id (car macros)) sym)
           (macro-emitter (car macros)))
          (else (loop (cdr macros))))))

(define (primitive? sym)
  (macro-lookup sym))

;; parse

(define (parse filename)
  (cons 'begin (file->list filename)))

;; load external resources

(define (external-load expr)
  (match expr
    (`(begin ,expr+ ...)
     `(begin ,@(map external-load expr+)))
    (`(require ,resources ...)
     `(begin ,@(map parse resources)))
    (else expr)))

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
                           (cons (make-variable id g 0)
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

;; global shake

(define (global-shake expr)
  (define (shake expr)
    (match expr
      (`(set! ,id ,_)
       (let ((b (global-var? id)))
         (if b
             (if (> (variable-refnum b) 0)
                 expr
                 (void))
             expr)))
      ((? list?)
       (map shake expr))
      (else expr)))
  (define (clean-void expr)
    (if (list? expr)
        (map clean-void
             (filter (lambda (x) (not (void? x))) expr))
        expr))
  (clean-void (shake expr)))

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
  (T*-k expr (lambda (x) `(%halt ,x))))

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
        (`(begin ,expr)
         (convert1 expr))
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

(define-struct instruction (opcode args))

(define (asm opcode . args)
  (make-instruction opcode args))

(define (instruction-flatten insts)
  (filter instruction? (flatten insts)))

(define (instruction-repr insts)
  (map (lambda (inst)
         (let ((opcode (instruction-opcode inst))
               (args (instruction-args inst)))
           (if (null? args)
               (list opcode)
               `(,opcode ,@args))))
       insts))

(define (emit expr fi env)
  (match expr
    ((? integer?)
     (asm 'load-integer expr))
    ((? boolean?)
     (asm 'load-boolean expr))
    ((? char?)
     (asm 'load-character expr))
    ((? symbol?)
     (cond ((lookup expr env)
            => (lambda (x)
                 (let ((y (binding-val x)))
                   (if (integer? y)
                       (asm 'get-frame-offset y)
                       (asm 'get-global-offset (cadr y))))))
           (else
            (error 'code-generate (format "undefined variable: ~a" expr)))))
    (`(begin ,expr+ ...)
     (map (lambda (e) (emit e fi env)) expr+))
    (`(if ,test ,conseq ,altern)
     (let ((alt-label (new-label 'L))
           (end-label (new-label 'L)))
       (list (emit test fi env)
             (asm 'jump-if-false alt-label)
             (emit conseq fi env)
             (asm 'jump end-label)
             (asm 'label alt-label)
             (emit altern fi env)
             (asm 'label end-label))))
    (`(let (,bindings ...) ,body)
     (let loop ((bindings bindings)
                (fi fi)
                (env env))
       (cond ((empty? bindings)
              (emit body fi env))
             (else
              (let* ((b (car bindings))
                     (id (car b))
                     (val (cadr b)))
                (list (emit val fi env)
                      (asm 'push-r)
                      (loop (cdr bindings)
                            (add1 fi)
                            (cons (make-binding id fi)
                                  env))))))))
    (`(lambda (,formals ...) ,body)
     (let ((code (new-label 'code))
           (end-label (new-label 'L)))
       (list (asm 'jump end-label)
             (asm 'label code)
             (let loop ((fmls (reverse formals))
                        (si -1)
                        (env env))
               (cond ((null? fmls)
                      (emit body fi env))
                     (else
                      (loop (cdr fmls)
                            (sub1 si)
                            (cons (make-binding (car fmls) si)
                                  env)))))
             (asm 'return)
             (asm 'label end-label)
             (asm 'load-label code))))
    (`(apply ,fn ,args)
     (emit `(,fn ,@args) fi env))
    (`(,(and prim (? primitive?)) ,args ...)
     ((macro-lookup prim) args fi env))
    (`(,fn ,args ...)
     (let ((num (length args))
           (bak 0))
       (list (let loop ((args args)
                        (fi fi))
               (cond ((null? args)
                      (begin
                        (list (emit fn fi env)
                              (asm 'adjust-frame-pointer fi)
                              (asm 'call-r)
                              (set! bak fi))))
                     (else
                      (begin
                        (list (emit (car args) fi env)
                              (asm 'push-r)
                              (loop (cdr args)
                                    (add1 fi)))))))
             (asm 'adjust-frame-pointer (- 0 bak)))))
    (else
     (error 'code-generate (format "unknown expression: ~a" expr)))))

(define (make-global-env)
  (let loop ((env *global-vars*)
             (index 0)
             (acc '()))
    (cond ((null? env) acc)
          (else (loop (cdr env)
                      (add1 index)
                      (cons (make-binding (binding-val (car env))
                                          `(global ,index))
                            acc))))))

(define (code-generate expr)
  ; frame pointer points return address
  ; frame index starts from 1
  (emit expr 1 (make-global-env)))

(define-syntax define-arithmetic
  (syntax-rules ()
    ((_ prim opcode)
     (define-primitive prim
       (lambda (args fi env)
         (if (= 2 (length args))
             (let ((lhs (car args))
                   (rhs (cadr args)))
               (list (emit rhs fi env)
                     (asm 'push-r)
                     (emit lhs (add1 fi) env)
                     (asm opcode)))
             (error (quote prim) (format "arity mismatch: ~a" args))))))))

(define-arithmetic + 'add-pop)
(define-arithmetic - 'subtract-pop)
(define-arithmetic * 'multiply-pop)
(define-arithmetic / 'divide-pop)

(define-primitive vector
  (lambda (args fi env)
    (let ((num (length args)))
      (if (> num 0)
          (list (asm 'allocate-heap num)
                (let loop ((args args)
                           (index 0))
                  (if (null? args)
                      (asm 'load-heap-pointer)
                      (list (emit (car args) fi env)
                            (asm 'set-heap-offset index)
                            (loop (cdr args)
                                  (add1 index))))))
          (error 'vector (format "arity mismatch: ~a" args))))))

(define-primitive vector-ref
  (lambda (args fi env)
    (if (= 2 (length args))
        (let ((ref (car args))
              (offset (cadr args)))
          (list (emit ref fi env)
                (asm 'store-heap-pointer)
                (emit offset fi env)
                (asm 'get-heap-r)))
        (error 'vector-ref (format "arity mismatch: ~a" args)))))

; our extended version
(define-primitive set!
  (lambda (args fi env)
    (if (= 2 (length args))
        (let ((target (car args))
              (value (cadr args)))
          (list (emit value fi env)
                (match target
                  ((? symbol?)
                   (cond ((lookup target env)
                          => (lambda (x)
                               (let ((y (binding-val x)))
                                 (if (integer? y)
                                     (asm 'set-frame-offset y)
                                     (asm 'set-global-offset (cadr y))))))))
                  ; generated by compiler
                  ; offset is guaranteed to be an immediate integer
                  (`(vector-ref ,ref ,offset)
                   (list (asm 'set-frame-offset fi)
                         (emit ref (add1 fi) env)
                         (asm 'store-heap-pointer)
                         (asm 'get-frame-offset fi)
                         (asm 'set-heap-offset offset)
                         (asm 'void)))
                  (else
                   (error 'set! (format "invalid target: ~a" target))))))
        (error 'set! (format "arity mismatch: ~a" args)))))

(define-primitive %halt
  (lambda (args fi env)
    (list (emit (car args) fi env)
          (asm 'halt))))

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
        external-load
        alpha-convert
        global-shake
        cps-convert
        closure-convert
        code-generate
        instruction-flatten
        instruction-repr
        pretty-print))

;; test

(compile "test.scm")
