#lang racket/base

(require racket/dict
         rackunit)

(define-syntax-rule (test f cases)
  (begin (require/expose "main.rkt" (f))
         (test-case
          (symbol->string (quote f))
          (for (((in want) (in-dict cases)))
            (let ((got (apply f in)))
              (check-equal? got want))))))

(test sum-largest-squares
      '(((1 2 3) . 13)
        ((4 3 2) . 25)
        ((5 3 4) . 41)))

(test sum-squares
      '(((1 2 3) . 14)
        ((4 3 2) . 29)
        ((5 3 4) . 50)))

(test remove-smallest
      '((((1 2 3)) . (2 3))
        (((4 3 2)) . (4 3))
        (((5 3 4)) . (5 4))))
