#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(define-check (check-all f d)
  (for (((args expected) (in-dict d)))
    (let ((actual (apply f args)))
      (with-check-info (('actual actual) ('expected expected))
        (unless (equal? actual expected) (fail-check))))))

(test-case
 "sum-largest-squares"
 (check-all sum-largest-squares
            '(((1 2 3) . 13)
              ((4 3 2) . 25)
              ((5 3 4) . 41))))
