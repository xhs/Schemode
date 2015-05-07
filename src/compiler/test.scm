(define fact1
  (lambda (n acc)
    (let ((i n)
          (acc 1))
      (if (= i 0)
          acc
          (fact1 (- i 1) (* acc i))))))
