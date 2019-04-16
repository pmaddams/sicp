#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "pascal"
 (for ((r (in-range 5))
       (l (in-list '((1)
                     (1 1)
                     (1 2 1)
                     (1 3 3 1)
                     (1 4 6 4 1)))))
   (check-equal? (for/list ((c (in-range (add1 r))))
                   (pascal r c))
                 l)))

(test-case
 "choose"
 (for ((n (in-range -1 5)))
   (for ((k (in-range -1 (+ n 2))))
     (check-equal? (choose n k) (pascal n k)))))
