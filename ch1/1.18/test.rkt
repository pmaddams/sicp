#lang racket/base

(require rackunit
         "main.rkt")

(define-check (check-op f reference)
  (for ((i (in-range 5)))
    (let* ((n (random 1 5))
           (m (random 1 5))
           (expected (reference n m))
           (actual (f n m)))
      (with-check-info (('actual actual) ('expected expected))
        (unless (= actual expected) (fail-check))))))

(test-case "add" (check-op add +))

(test-case "sub" (check-op sub -))

(test-case "mul" (check-op mul *))

(test-case "div" (check-op div quotient))

(test-case "pow" (check-op pow expt))
