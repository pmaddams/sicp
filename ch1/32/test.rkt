#lang racket/base

(require racket/function
         rackunit
         "main.rkt")

(define (square n) (* n n))

(define (add2 n) (+ n 2))

(test-case
 "sum"
 (for ((i (in-range 5)))
   (let* ((a (random 1 10))
          (b (random a 100)))
     (check-equal? (sum identity a add1 b)
                   (for/sum ((i (in-range a (add1 b))))
                     i))
     (check-equal? (sum square a add2 b)
                   (for/sum ((i (in-range a (add1 b) 2)))
                     (square i))))))

(test-case
 "product"
 (for ((i (in-range 5)))
   (let* ((a (random 1 10))
          (b (random a 100)))
     (check-equal? (product identity a add1 b)
                   (for/product ((i (in-range a (add1 b))))
                     i))
     (check-equal? (product square a add2 b)
                   (for/product ((i (in-range a (add1 b) 2)))
                     (square i))))))
