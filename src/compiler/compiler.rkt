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
  (or (member sym '(begin if))
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

(define (cpsify prim)
  (string->symbol
   (string-append "%"
                  (symbol->string prim))))

(define (uncpsify prim)
  (string->symbol
   (substring (symbol->string prim) 1)))

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
         ,$cont)))
    (`(let ((,ids ,vals) ...) ,body)
     (T*-k vals (lambda ($vals)
                  `(let (,@(map list ids $vals))
                     ,(T-c body cont)))))
    (`(,(and prim (? primitive?)) ,args ...)
     (T*-k args (lambda ($args)
                  `(,(cpsify prim) ,cont ,@$args))))
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

(define (closure-convert expr)
  (let ((codes '()))
    (define (convert expr)
      (match expr
        (`(lambda (,formals ...) ,body)
         (let ((code (new-label 'code))
               (fvs (keep (lambda (x) (not (global-var? x)))
                          (free-variables expr))))
           (set! codes
                 (cons (list code
                             `(code ,formals ,fvs ,(convert body)))
                       codes))
           `(closure ,code ,fvs)))
        (`(let ((,ids ,vals) ...) ,body)
         `(let (,@(map list ids (map convert vals)))
            ,(convert body)))
        ((? list?)
         (map convert expr))
        (else expr)))
    (let ((body (convert expr)))
      (let loop ((codes codes)
                 (body body))
        (if (null? codes)
            body
            (loop (cdr codes)
                  `(let (,(car codes))
                     ,body)))))))

;; compile

(define-struct instruction (opcode operands))

(define (asm opcode . args)
  (make-instruction opcode args))

(define (instruction-flatten insts)
  (map (lambda (inst)
         `(,(instruction-opcode inst) ,(instruction-operands inst)))
       (filter instruction? (flatten insts))))

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
                   (match y
                     (`(global ,offset)
                      (asm 'get-global-offset offset))
                     (`(frame ,offset)
                      (asm 'get-frame-offset offset))
                     (`(closure ,offset)
                      (asm 'get-closure-offset offset))
                     (`(stack ,offset)
                      (asm 'get-stack-offset offset))))))
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
                            (cons (make-binding id `(stack ,fi))
                                  env))))))))
    (`(code (,formals ...) (,fvs ...) ,body)
     (let ((code-label (new-label 'L))
           (end-label (new-label 'L)))
       (list (asm 'jump end-label)
             (asm 'label code-label)
             (emit body
                   1 ; new frame index starts from 1
                   (append (map make-binding
                                formals
                                (map (lambda (fi) `(frame ,fi))
                                     (map (lambda (v) (sub1 v))
                                          (range (- 0 (length formals)) 0))))
                           (map make-binding
                                fvs
                                (map (lambda (ci) `(closure ,ci))
                                     (map (lambda (v) (add1 v))
                                          (range 0 (length fvs)))))
                           env))
             (asm 'return)
             (asm 'label end-label)
             (asm 'load-label code-label))))
    (`(closure ,label ,fvs)
     (list (asm 'allocate-heap (+ 1 (length fvs)))
           (emit label fi env)
           (asm 'set-heap-offset 0)
           (let loop ((fvs fvs)
                      (index 1))
             (if (null? fvs)
                 (asm 'load-heap-pointer)
                 (list (emit (car fvs) fi env)
                       (asm 'set-heap-offset index)
                       (loop (cdr fvs)
                             (add1 index)))))))
    (`(,(and prim (? primitive?)) ,args ...)
     ((macro-lookup prim) args fi env))
    (`(,fn ,args ...)
     (let ((num (length args)))
       (let loop ((args args)
                  (fi fi))
         (if (null? args)
             (list (emit fn fi env)
                   (asm 'store-closure-pointer)
                   (asm 'get-closure-offset 0)
                   (asm 'call-r)
                   (asm 'adjust-stack-pointer (- 0 num)))
             (list (emit (car args) fi env)
                   (asm 'push-r)
                   (loop (cdr args)
                         (add1 fi)))))))
    (else
     (error 'code-generate (format "unknown expression: ~a" expr)))))

(define (make-global-env)
  (let loop ((env
              (keep (lambda (g) (> (variable-refnum g) 0))
                    *global-vars*))
             (index 0)
             (acc '()))
    (cond ((null? env) acc)
          (else (loop (cdr env)
                      (add1 index)
                      (cons (make-binding (binding-val (car env))
                                          `(global ,index))
                            acc))))))

(define (code-generate expr)
  ; frame pointer points to return address
  ; frame index starts from 1
  (list (emit expr 1 (make-global-env))
        (asm 'halt)))

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

(define-syntax define-cps-primitive
  (syntax-rules ()
    ((_ cps-prim prim)
     (let ((emitter (lambda (args fi env)
                      (let ((cont (car args))
                            (args (cdr args)))
                        (list ((macro-lookup (quote prim)) args fi env)
                              (asm 'push-r)
                              (emit cont (add1 fi) env)
                              (asm 'store-closure-pointer)
                              (asm 'get-closure-offset 0)
                              (asm 'call-r)
                              (asm 'adjust-stack-pointer -1))))))
       (set! *primitives*
             (cons (make-macro (quote cps-prim) emitter)
                   *primitives*))))))

(define-cps-primitive %+ +)

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
                               (list (let ((y (binding-val x)))
                                       (match y
                                         (`(global ,offset)
                                          (asm 'set-global-offset offset))
                                         (`(frame ,offset)
                                          (asm 'set-frame-offset offset))
                                         (`(closure ,offset)
                                          (asm 'set-closure-offset offset))
                                         (`(stack ,offset)
                                          (asm 'set-stack-offset offset))))
                                     (asm 'void))))))
                  (else
                   (error 'set! (format "invalid target: ~a" target))))))
        (error 'set! (format "arity mismatch: ~a" args)))))

(define-cps-primitive %set! set!)

(define-primitive halt
  (lambda (args fi env)
    (list (emit (car args) fi env)
          (asm 'halt))))

;; assemble

(define *asms* '())

(define-syntax define-asm
  (syntax-rules ()
    ((_ opcode hexifier)
     (set! *asms*
           (cons (make-binding (quote opcode) hexifier)
                 *asms*)))))

(define $and bitwise-and)
(define $or bitwise-ior)
(define $xor bitwise-xor)
(define $shift arithmetic-shift)

(define *label-offset* 0)
(define *labels* '())

(define (grow size)
  (set! *label-offset*
        (+ size *label-offset*)))

(define (make-label label)
  (set! *labels*
        (cons (make-binding label *label-offset*)
              *labels*)))

(define-struct hex ())
(define-struct (byte hex) (val))
(define-struct (word hex) (val))
(define-struct (dword hex) (val))

(define-asm void
  ; 1111_1111
  (lambda (_)
    (grow 1)
    `(,(make-byte #b11111111))))

(define-asm load-integer
  ; 0000_0000 iiii_iiii
  ; 0000_0001 xiii_iiii iiii_iiii iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((int (car operands)))
      (if (or (> int 127)
              (< int -128))
          (begin
            (grow 5)
            `(,(make-byte #b00000001)
              ,(make-dword int)))
          (begin
            (grow 2)
            `(,(make-byte #b00000000)
              ,(make-byte int)))))))

(define-asm load-boolean
  ; 0000_001i
  (lambda (operands)
    (grow 1)
    (if (car operands)
        `(,(make-byte #b00000011))
        `(,(make-byte #b00000010)))))

(define-asm load-character
  ; 0000_0100 iiii_iiii
  (lambda (operands)
    (grow 2)
    `(,(make-byte #b00000100)
      ,(make-byte (char->integer (car operands))))))

(define-asm push-r
  ; 0000_0101
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00000101))))

(define-asm set-global-offset
  ; 0000_0110 iiii_iiii
  ; 0000_0111 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (> offset 255)
          (begin
            (grow 3)
            `(,(make-byte #b00000111) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00000110) ,(make-byte offset)))))))

(define-asm get-global-offset
  ; 0000_1000 iiii_iiii
  ; 0000_1001 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (> offset 255)
          (begin
            (grow 3)
            `(,(make-byte #b00001001) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00001000) ,(make-byte offset)))))))

(define-asm set-frame-offset
  ; 0000_1010 iiii_iiii
  ; 0000_1011 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (or (> offset 127)
              (< offset -128))
          (begin
            (grow 3)
            `(,(make-byte #b00001011) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00001010) ,(make-byte offset)))))))

(define-asm get-frame-offset
  ; 0000_1100 iiii_iiii
  ; 0000_1101 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (or (> offset 127)
              (< offset -128))
          (begin
            (grow 3)
            `(,(make-byte #b00001101) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00001100) ,(make-byte offset)))))))

(define-asm allocate-heap
  ; 0000_1110 iiii_iiii
  ; 0000_1111 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((size (car operands)))
      (if (> size 255)
          (begin
            (grow 3)
            `(,(make-byte #b00001111) ,(make-word size)))
          (begin
            (grow 2)
            `(,(make-byte #b00001110) ,(make-byte size)))))))

(define-asm set-heap-offset
  ; 0001_0000 iiii_iiii
  ; 0001_0001 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (> offset 255)
          (begin
            (grow 3)
            `(,(make-byte #b00010001) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00010000) ,(make-byte offset)))))))

(define-asm get-heap-r
  ; 0001_0010
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00010010))))

(define-asm load-heap-pointer
  ; 0001_0011
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00010011))))

(define-asm store-heap-pointer
  ; 0001_0100
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00010100))))

(define-asm adjust-stack-pointer
  ; 0001_0101 iiii_iiii
  ; 0001_0110 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((adj (car operands)))
      (if (or (> adj 127)
              (< adj -128))
          (begin
            (grow 3)
            `(,(make-byte #b00010110) ,(make-word adj)))
          (begin
            (grow 2)
            `(,(make-byte #b00010101) ,(make-byte adj)))))))

(define-asm call-r
  ; 0001_0111
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00010111))))

(define-asm return
  ; 0001_1000
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00011000))))

(define-asm halt
  ; 0001_1001
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00011001))))

(define-asm add-pop
  ; 0001_1010
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00011010))))

(define-asm subtract-pop
  ; 0001_1011
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00011011))))

(define-asm multiply-pop
  ; 0001_1100
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00011100))))

(define-asm divide-pop
  ; 0001_1101
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00011101))))

(define-asm load-closure-pointer
  ; 0010_0001
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00100001))))

(define-asm store-closure-pointer
  ; 0010_0010
  (lambda (_)
    (grow 1)
    `(,(make-byte #b00100010))))

(define-asm set-closure-offset
  ; 0010_0011 iiii_iiii
  ; 0010_0100 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (> offset 255)
          (begin
            (grow 3)
            `(,(make-byte #b00100100) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00100011) ,(make-byte offset)))))))

(define-asm get-closure-offset
  ; 0010_0101 iiii_iiii
  ; 0010_0110 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (> offset 255)
          (begin
            (grow 3)
            `(,(make-byte #b00100110) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00100101) ,(make-byte offset)))))))

(define-asm set-stack-offset
  ; 0010_0111 iiii_iiii
  ; 0010_1000 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (> offset 255)
          (begin
            (grow 3)
            `(,(make-byte #b00101000) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00100111) ,(make-byte offset)))))))

(define-asm get-stack-offset
  ; 0010_1001 iiii_iiii
  ; 0010_1010 iiii_iiii iiii_iiii
  (lambda (operands)
    (let ((offset (car operands)))
      (if (> offset 255)
          (begin
            (grow 3)
            `(,(make-byte #b00101010) ,(make-word offset)))
          (begin
            (grow 2)
            `(,(make-byte #b00101001) ,(make-byte offset)))))))

(define (assemble insts)
  (define (assemble-inst inst)
    (let ((opcode (car inst))
          (operands (cadr inst)))
      (cond ((lookup opcode *asms*)
             => (lambda (a)
                  ((binding-val a) operands)))
            ((eq? opcode 'label)
             (begin
               (make-label (car operands))
               (void)))
            (else
             (begin (grow 5) inst)))))
  (define (assemble-label inst)
    (match inst
      (`(load-label (,label))
       ; 0001_1110 iiii_iiii iiii_iiii iiii_iiii iiii_iiii
       (cond ((lookup label *labels*)
              => (lambda (l)
                   (let ((dest (binding-val l)))
                     `(,(make-byte #b00011110) ,(make-dword dest)))))))
      (`(jump (,label))
       ; 0001_1111 iiii_iiii iiii_iiii iiii_iiii iiii_iiii
       (cond ((lookup label *labels*)
              => (lambda (l)
                   (let ((dest (binding-val l)))
                     `(,(make-byte #b00011111) ,(make-dword dest)))))))
      (`(jump-if-false (,label))
       ; 0010_0000 iiii_iiii iiii_iiii iiii_iiii iiii_iiii
       (cond ((lookup label *labels*)
              => (lambda (l)
                   (let ((dest (binding-val l)))
                     `(,(make-byte #b00100000) ,(make-dword dest)))))))
      (else inst)))
  (define (hexify hex)
    (cond ((byte? hex)
           (byte-val hex))
          ((word? hex)
           (let ((v (word-val hex)))
             `(,($shift v -8)
               ,($and #xff v))))
          ((dword? hex)
           (let ((v (dword-val hex)))
             `(,($shift v -24)
               ,($and #xff ($shift v -16))
               ,($and #xff ($shift v -8))
               ,($and #xff v))))))
  (flatten
   (map hexify
        (flatten
         (filter (lambda (x) (not (void? x)))
                 (map assemble-label
                      (map assemble-inst insts)))))))

;; output

(define (crc32 datum)
  ($xor (for/fold ((acc #xffffffff))
                  ((data datum))
          (for/fold ((acc ($xor acc data)))
                    ((num (in-range 0 8)))
            ($xor (quotient acc 2)
                  (* #xedb88320 ($and acc 1)))))
        #xffffffff))

(define (write-dword dw out)
  (write-byte ($shift dw -24) out)
  (write-byte ($and #xff ($shift dw -16)) out)
  (write-byte ($and #xff ($shift dw -8)) out)
  (write-byte ($and #xff dw) out))

(define (binary-output datum)
  (let ((out (open-output-file "output.bin"
                               #:mode 'binary #:exists 'replace))
        (datum (map (lambda (b)
                      (if (< b 0) (+ 256 b) b))
                    datum)))
    (write-string "Schemode" out) ; magic header
    (write-dword 1 out) ; version number
    (write-dword (length datum) out) ; payload length
    (write-dword (crc32 datum) out) ; CRC32 checksum
    (let loop ((datum datum))
      (if (null? datum)
          (close-output-port out)
          (let* ((b (car datum)))
            (write-byte b out)
            (loop (cdr datum)))))))

;; test

(define repr-b
  (lambda (b)
    `(,(binding-id b) ,(binding-val b))))

(define repr-v
  (lambda (v)
    `(,(binding-id v) ,(variable-refnum v))))

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
        ;code-generate
        ;instruction-flatten
        ;assemble
        ;binary-output
        pretty-print))

(compile "test.scm")
