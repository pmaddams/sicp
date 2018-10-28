#lang racket/base

(require rackunit
         "main.rkt")

(define primes '(2 3 5 7 11 13 17 23 29 31))

(define composites '(0 1 4 6 8 9 10 12 14 15))

(define-check (check-prime? f)
  (let ((prime? (make-prime? f)))
    (for ((n primes))
      (with-check-info (('actual n) ('expected 'prime))
        (unless (prime? n) (fail-check))))
    (for ((n composites))
      (with-check-info (('actual n) ('expected 'composite))
        (when (prime? n)  (fail-check))))))

(test-case
 "trial-division"
 (check-prime? trial-division))

(test-case
 "fermat"
 (check-prime? (fermat 10)))

(test-case
 "expmod"
 (for ((i (in-range 10)))
   (let ((b (random 2 100))
         (x (random 2 100))
         (m (random 2 100)))
     (check-eq? (expmod b x m)
                (modulo (expt b x) m)))))

(test-case
 "miller-rabin"
 (check-prime? (miller-rabin 10)))

(test-case
 "carmichael numbers"
 (for ((n '(561 1105 1729 2465 2821 6601)))
   (check-true (fools-fermat? n)
               (format "~a doesn't fool fermat" n))
   (check-false (fools-miller-rabin? n)
                (format "~a fools miller-rabin" n))))
