#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "basic tests"
 (check-equal? (church->number zero) 0)
 (check-equal? (church->number one) 1)
 (check-equal? (church->number two) 2)
 (check church=? (number->church 0) zero)
 (check church=? (number->church 1) one)
 (check church=? (number->church 2) two)
 (check church=? (add one zero) one)
 (check church=? (add one one) two))

(test-case
 "random tests"
 (for ((i 5))
   (let ((n (random 10)))
     (check-equal? (church->number (number->church n)) n)
     (check church=?
            (add one (number->church n))
            (number->church (add1 n))))))

(test-case
 "add"
 (for ((i 5))
   (let ((n (random 10))
         (m (random 10)))
     (check church=?
            (add (number->church n) (number->church m))
            (number->church (+ n m))))))

(test-case
 "mul"
 (for ((i 5))
   (let ((n (random 10))
         (m (random 10)))
     (check church=?
            (mul (number->church n) (number->church m))
            (number->church (* n m))))))

(test-case
 "pow"
 (for ((i 5))
   (let ((n (random 2 5))
         (m (random 2 5)))
     (check church=?
            (pow (number->church n) (number->church m))
            (number->church (expt n m))))))
