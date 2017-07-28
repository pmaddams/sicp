#lang sicp

(define (repeated f n)
  (let ((double (lambda (f)
                  (lambda (x)
                    (f (f x))))))
    (if (= n 1)
        f
        (repeated (double f) (dec n)))))

(let ((square (lambda (x)
                (expt x 2))))
  ((repeated square 2) 5))
;; 625