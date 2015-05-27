(let ((foo (let ((x 1))
             (lambda (y) (+ x y)))))
  (foo 100)
  (foo 200))
