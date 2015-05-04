(define x 100)

(define foo
  (lambda (x) (* x x)))

(define bar
  (lambda (x) (+ 1 x)))

(bar (foo 4))
