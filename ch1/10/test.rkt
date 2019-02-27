#lang racket/base

(require rackunit
         "main.rkt")

(define-check (check-A i f)
  (for ((n 5))
    (let ((expected (A i n))
          (actual (f n)))
      (with-check-info (('actual actual) ('expected expected))
        (unless (= actual expected) (fail-check))))))

(test-case "f" (check-A 0 f))

(test-case "g" (check-A 1 g))

(test-case "h" (check-A 2 h))
