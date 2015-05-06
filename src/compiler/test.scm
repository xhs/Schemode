(lambda (x y z)
  (let ((f
    (lambda
      (a b)
      (+ (* a x) (* b y)))))
  (- (f 1 2) (f 3 4))))
