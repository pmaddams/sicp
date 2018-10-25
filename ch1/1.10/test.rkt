#lang racket/base

(require rackunit
         "main.rkt")

(define-check (check-A i f seq)
  (for ((n seq))
    (let ((m (A i n))
          (p (f n)))
      (unless (= m p)
        (fail-check (format "(A ~a ~a) is ~a, not ~a" i n m p))))))

(test-case "f" (check-A 0 f (in-range 15)))

(test-case "g" (check-A 1 g (in-range 10)))

(test-case "h" (check-A 2 h (in-range 5)))
