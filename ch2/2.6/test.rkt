#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "basic tests"
 (check = (church->number church-0) 0)
 (check = (church->number church-1) 1)
 (check = (church->number church-2) 2)
 (check church-= (number->church 0) church-0)
 (check church-= (number->church 1) church-1)
 (check church-= (number->church 2) church-2)
 (check church-= (church-add1 church-0) church-1)
 (check church-= (church-add1 church-1) church-2))

(test-case
 "random tests"
 (for ((i (in-range 5)))
   (let ((n (random 10)))
     (check = (church->number (number->church n)) n)
     (check church-=
            (church-add1 (number->church n))
            (number->church (add1 n))))))

(test-case
 "church-add"
 (for ((i (in-range 5)))
   (let ((n (random 10))
         (m (random 10)))
     (check church-=
            (church-add (number->church n) (number->church m))
            (number->church (+ n m))))))

(test-case
 "church-mul"
 (for ((i (in-range 5)))
   (let ((n (random 10))
         (m (random 10)))
     (check church-=
            (church-mul (number->church n) (number->church m))
            (number->church (* n m))))))

(test-case
 "church-expt"
 (for ((i (in-range 5)))
   (let ((n (random 2 5))
         (m (random 2 5)))
     (check church-=
            (church-expt (number->church n) (number->church m))
            (number->church (expt n m))))))
