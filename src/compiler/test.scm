(define x 100)

(define adder
  (lambda (x)
    (lambda (y) (+ x y))))

(define foo (adder 1))

(foo x)
