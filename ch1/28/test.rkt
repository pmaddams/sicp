#lang racket/base

(require rackunit
         "main.rkt")

(define primes '(2 3 5 7 11 13 17 23 29 31))

(define composites '(0 1 4 6 8 9 10 12 14 15))

(define-check (check-prime p)
  (for ((n (in-list primes)))
    (with-check-info (('actual n) ('expected 'prime))
      (unless (p n) (fail-check))))
  (for ((n (in-list composites)))
    (with-check-info (('actual n) ('expected 'composite))
      (when (p n) (fail-check)))))

(test-case
 "trial-division"
 (check-prime trial-division))

(test-case
 "expmod"
 (for ((i (in-range 10)))
   (let ((b (random 2 100))
         (x (random 2 100))
         (m (random 2 100)))
     (check-eq? (expmod b x m)
                (modulo (expt b x) m)))))

(test-case
 "fermat"
 (check-prime (fermat 10)))

(test-case
 "miller-rabin"
 (check-prime (miller-rabin 10)))

(test-case
 "carmichael numbers"
 (for ((n (in-list '(561 1105 1729 2465 2821 6601))))
   (check-true (fools-fermat n)
               (format "~a doesn't fool fermat" n))
   (check-false (fools-miller-rabin n)
                (format "~a fools miller-rabin" n))))
