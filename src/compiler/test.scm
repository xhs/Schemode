(let ((x 1) (y 2))
  ((lambda (a b)
     (quote 'foobar)
     (+ a b)) x y))