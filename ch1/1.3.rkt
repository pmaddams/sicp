#lang racket/base

(require racket/dict
         rackunit)

(module+ test
  (define (all-equal? f cases)
    (for (((in want) (in-dict cases)))
      (let ((got (apply f in)))
        (check-equal? got want)))))

(define (sum-largest-squares . l)
  (apply sum-squares (remove-smallest l)))

(module+ test
  (all-equal? sum-largest-squares
              '(((1 2 3) . 13)
                ((4 3 2) . 25)
                ((5 3 4) . 41))))

(define (sum-squares . l)
  (apply + (map square l)))

(module+ test
  (all-equal? sum-squares
              '(((1 2 3) . 14)
                ((4 3 2) . 29)
                ((5 3 4) . 50))))

(define (remove-smallest l)
  (let ((smallest (apply min l)))
    (remove smallest l)))

(module+ test
  (all-equal? remove-smallest
              '((((1 2 3)) . (2 3))
                (((4 3 2)) . (4 3))
                (((5 3 4)) . (5 4)))))

(define (square n) (* n n))
