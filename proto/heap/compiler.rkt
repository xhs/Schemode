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
  (lambda (x e next)
     (cond
        [(symbol? x)
         (list 'refer (compile-lookup x e) next)]
        [(pair? x)
         (record-case x
            [quote (obj)
             (list 'constant obj next)]
            [lambda (vars body)
             (list 'close
                   (compile body (extend e vars) '(return))
                   next)]
            [if (test then else)
             (let ([thenc (compile then e next)]
                   [elsec (compile else e next)])
                (compile test e (list 'test thenc elsec)))]
            [set! (var x)
             (let ([access (compile-lookup var e)])
                (compile x e (list 'assign access next)))]
            [call/cc (x)
             (let ([c (list 'conti
                            (list 'argument
                                  (compile x e '(apply))))])
                (if (tail? next)
                    c
                    (list 'frame next c)))]
            [else
             (recur loop ([args (cdr x)]
                          [c (compile (car x) e '(apply))])
                (if (null? args)
                    (if (tail? next)
                        c
                        (list 'frame next c))
                    (loop (cdr args)
                          (compile (car args)
                                    e
                                    (list 'argument c)))))])]
        [else
         (list 'constant x next)])))

(define lookup
  (lambda (access e)
    (recur nxtrib ([e e] [rib (car access)])
      (if (= rib 0)
          (recur nxtelt ([r (car e)] [elt (cdr access)])
            (if (= elt 0)
                r
                (nxtelt (cdr r) (- elt 1))))
          (nxtrib (cdr e) (- rib 1))))))

(define closure
  (lambda (body e)
    (list body e)))

(define continuation
  (lambda (s)
    (closure (list 'nuate s '(0 . 0)) '())))

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

(define extend
  (lambda (e r)
    (cons r e)))

(define compile-lookup
  (lambda (var e)
    (recur nxtrib ([e e] [rib 0])
      (recur nxtelt ([vars (car e)] [elt 0])
        (cond
          [(null? vars) (nxtrib (cdr e) (+ rib 1))]
          [(eq? (car vars) var) (cons rib elt)]
          [else (nxtelt (cdr vars) (+ elt 1))])))))

(define set-car!
  (lambda (p v)
    (cons v (cdr p))))

(define VM
  (lambda (a x e r s)
    (record-case x
      [halt () a]
      [refer (var x)
       (VM (car (lookup var e)) x e r s)]
      [constant (obj x)
       (VM obj x e r s)]
      [close (body x)
       (VM (closure body e) x e r s)]
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
       (record (body e) a
          (VM a body (extend e r) '() s))]
      [return ()
       (record (x e r s) s
          (VM a x e r s))])))

(define evaluate
  (lambda (x)
     (VM '() (compile x '() '(halt)) '() '() '())))

(evaluate '((lambda (x) (+ 1 x)) 1))
