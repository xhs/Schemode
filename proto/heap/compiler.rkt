#lang racket

(define-syntax rec
  (syntax-rules ()
    ((_ var exp)
     (letrec ((var exp)) var))))

(define-syntax recur
  (syntax-rules ()
    ((_ fn ([var init] ...) exp ...)
     ((rec fn (lambda (var ...) exp ...)) init ...))))

(define-syntax record
  (syntax-rules ()
    ((_ (var ...) val exp ...)
     (apply (lambda (var ...) exp ...) val))))

(define-syntax (record-case stx)
  (syntax-case stx (else)
    [(src-record-case exp1
                      [else exp3 ...])
     (syntax/loc stx (begin exp3 ...))]
    [(src-record-case exp1
                      [key vars exp2 ...])
     (identifier? (syntax exp1))
     (syntax/loc stx (if (eq? (car exp1) 'key)
                         (record vars (cdr exp1) exp2 ...)
                         (raise-syntax-error #f "no matching clause" (syntax src-record-case))))]
    [(src-record-case exp1
                      [key vars exp2 ...]
                      clause
                      ...)
     (identifier? (syntax exp1))
     (syntax/loc stx (if (eq? (car exp1) 'key)
                         (record vars (cdr exp1) exp2 ...)
                         (record-case exp1
                                      clause
                                      ...)))]
    [(src-record-case exp1
                      clause
                      ...)
     (not (identifier? (syntax exp1)))
     (let ([r (car (generate-temporaries (datum->syntax (syntax src-record-case) '(q))))])
       (quasisyntax/loc (syntax src-record-case)
                        (let ([(unsyntax r) exp1])
                          (record-case (unsyntax r)
                                       clause
                                       ...))))]))

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

(define compile
  (lambda (x next)
     (cond
        [(symbol? x)
         (list 'refer x next)]
        [(pair? x)
         (record-case x
            [quote (obj)
             (list 'constant obj next)]
            [lambda (vars body)
             (list 'close vars (compile body '(return)) next)]
            [if (test then else)
             (let ([thenc (compile then next)]
                   [elsec (compile else next)])
                (compile test (list 'test thenc elsec)))]
            [set! (var x)
             (compile x (list 'assign var next))]
            [define (var x)
             (compile x (list 'assign var next))]
            [call/cc (x)
             (let ([c (list 'conti
                            (list 'argument
                                  (compile x '(apply))))])
                (if (tail? next)
                    c
                    (list 'frame next c)))]
            [else
             (recur loop ([args (cdr x)]
                          [c (compile (car x) '(apply))])
                (if (null? args)
                    (if (tail? next)
                        c
                        (list 'frame next c))
                    (loop (cdr args)
                          (compile (car args)
                                   (list 'argument c)))))])]
        [else
         (list 'constant x next)])))


(define lookup
  (lambda (var e)
    (recur nxtrib ([e e])
      (recur nxtelt ([vars (caar e)]
                     [vals (cdar e)])
        (cond
          [(null? vars) (nxtrib (cdr e))]
          [(eq? (car vars) var) vals]
          [else (nxtelt (cdr vars) (cdr vals))])))))

(define closure
  (lambda (body e vars)
    (list body e vars)))

(define continuation
  (lambda (s)
    (closure (list 'nuate s 'v) '() '(v))))

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

(define extend
  (lambda (e vars vals)
    (cons (cons vars vals) e)))

(define set-car!
  (lambda (p v)
    (cons v (cdr p))))

(define VM
  (lambda (a x e r s)
    (display (list a x e r s))
    (newline)
    (record-case x
      [halt () a]
      [refer (var x)
       (VM (car (lookup var e)) x e r s)]
      [constant (obj x)
       (VM obj x e r s)]
      [close (vars body x)
       (VM (closure body e vars) x e r s)]
      [test (then else)
       (VM a (if a then else) e r s)]
      [assign (var x)
       (set-car! (lookup var e) a)
       (VM a x e r s)]
      [conti (x)
       (VM (continuation s) x e r s)]
      [nuate (s var)
       (VM (car (lookup var e)) '(return) e r s)]
      [frame (ret x)
       (VM a x e '() (call-frame ret e r s))]
      [argument (x)
       (VM a x e (cons a r) s)]
      [apply ()
       (record (body e vars) a
          (VM a body (extend e vars r) '() s))]
      [return ()
       (record (x e r s) s
          (VM a x e r s))])))

(define evaluate
  (lambda (x)
     (VM '() (compile x '(halt)) '() '() '())))

(evaluate '((lambda (x) (+ 1 x)) 1))
