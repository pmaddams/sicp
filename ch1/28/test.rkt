#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "make-prime"
 (for ((p (in-list (list trial-division
                         (fermat 10)
                         (miller-rabin 10)))))
   (for ((n (in-list '(2 3 5 7 11 13 17 23 29 31))))
     (check-true (p n)))
   (for ((n (in-list '(0 1 4 6 8 9 10 12 14 15))))
     (check-false (p n)))))

(test-case
 "mod-expt"
 (for ((i (in-range 10)))
   (let ((b (random 2 100))
         (n (random 2 100))
         (m (random 2 100)))
     (check-equal? (mod-expt b n m) (modulo (expt b n) m)))))

(test-case
 "carmichael numbers"
 (for ((n (in-list '(561 1105 1729 2465 2821 6601))))
   (check-true (fools-fermat n))
   (check-false (fools-miller-rabin n))))
