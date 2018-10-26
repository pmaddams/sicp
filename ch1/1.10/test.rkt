#lang racket/base

(require rackunit
         "main.rkt")

(define-check (check-A i f)
  (for ((n (in-range 5)))
    (let ((m (A i n))
          (p (f n)))
      (unless (= m p)
        (fail-check (format "(A ~a ~a) -> ~a, got ~a" i n m p))))))

(test-case "f" (check-A 0 f))

(test-case "g" (check-A 1 g))

(test-case "h" (check-A 2 h))
