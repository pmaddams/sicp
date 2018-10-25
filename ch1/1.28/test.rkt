#lang racket/base

(require racket/dict
         rackunit
         "main.rkt")

(define (check-prime? f)
  (let ((prime? (make-prime? f))
        (primes '(2 3 5 7 11 13))
        (composites '(0 1 4 6 8 9)))
    (for ((n primes))
      (check-true (prime? n)))
    (for ((n composites))
      (check-false (prime? n)))))

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
 "carmichaels"
 (let ((carmichaels '(561 1105 1729 2465 2821 6601)))
   (for ((n carmichaels))
     (check-true (fools-fermat? n))
     (check-false (fools-miller-rabin? n)))))
