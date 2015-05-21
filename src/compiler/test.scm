(let ((x 1) (y 2))
  ((lambda (a b)
     "foobar"
     (quote 'foobar)
     "foobar"
     (+ a b)) x y))