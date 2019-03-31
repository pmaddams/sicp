#lang racket/base

(require racket/function
         rackunit
         "main.rkt")

(define (square n) (* n n))

(test-case
 "sum"
 (for ((i (in-range 5)))
   (for ((term (in-list (list identity square))))
     (let* ((n (random 1 5))
            (next (lambda (i) (+ i n)))
            (a (random 1 10))
            (b (random a 100)))
       (check-equal? (sum term a next b)
                     (for/sum ((i (in-range a (add1 b) n)))
                       (term i)))))))

(test-case
 "product"
 (for ((i (in-range 5)))
   (for ((term (in-list (list identity square))))
     (let* ((n (random 1 5))
            (next (lambda (i) (+ i n)))
            (a (random 1 10))
            (b (random a 100)))
       (check-equal? (product term a next b)
                     (for/product ((i (in-range a (add1 b) n)))
                       (term i)))))))
