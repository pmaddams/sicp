#lang racket/base

(require rackunit
         "main.rkt")

(define-check (check-op f1 f2)
  (for ((i (in-range 5)))
    (let* ((n (random 1 5))
           (m (random 1 5))
           (v1 (f1 n m))
           (v2 (f2 n m)))
      (unless (= v1 v2)
        (fail-check (format "(~a ~a ~a) -> ~a, (~a ~a ~a) -> ~a"
                            f1 n m v1 f2 n m v2))))))

(test-case "add" (check-op add +))

(test-case "sub" (check-op sub -))

(test-case "mul" (check-op mul *))

(test-case "div" (check-op div quotient))

(test-case "pow" (check-op pow expt))
