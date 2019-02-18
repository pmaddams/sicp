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
            `(((1 2 3) . ,(sum-of-squares 2 3))
              ((4 3 5 2) . ,(sum-of-squares 4 3 5))
              ((6 7 3 4 5) . ,(sum-of-squares 6 7 4 5)))))

(test-case
 "remove-smallest"
 (check-all remove-smallest
            '((((1 2 3)) . (2 3))
              (((4 3 5 2)) . (4 3 5))
              (((6 7 3 4 5)) . (6 7 4 5)))))
