(define bar
  (let ((count 0))
    (lambda ()
      (set! count (+ 1 2 count))
      count)))

(bar)
