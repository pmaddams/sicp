#lang sicp

(define (square x)
  (expt x 2))

(define (divides? a b)
  (zero? (remainder b a)))

(define (smallest-divisor n)
  (letrec ((s (lambda (i)
                (cond ((> (square i) n) n)
                      ((divides? i n) i)
                      (else (s (+ i 2)))))))
    (s 3)))

(define (prime? n)
  (and (> n 1)
       (not (= n 2))
       (odd? n)
       (= n (smallest-divisor n))))

(define (timed-prime-test n)
  (let ((start (runtime)))
    (if (prime? n)
        (let ((elapsed (- (runtime) start)))
          (display n)
          (display " *** ")
          (display elapsed)
          (newline)
          elapsed))))

(define (search-for-primes n lim)
  (letrec ((s (lambda (n)
                (if (< n lim)
                    (begin (timed-prime-test n)
                           (s (+ n 2)))))))
    (if (even? n)
        (set! n (inc n)))
    (s n)))

(search-for-primes 1000 1020)
;; 1009 *** 21
;; 1013 *** 21
;; 1019 *** 22

(search-for-primes 10000 10040)
;; 10007 *** 60
;; 10009 *** 76
;; 10037 *** 60

(search-for-primes 100000 100050)
;; 100003 *** 170
;; 100019 *** 170
;; 100043 *** 170

(search-for-primes 1000000 1000040)
;; 1000003 *** 531
;; 1000033 *** 528
;; 1000037 *** 529

;; The improved smallest-divisor procedure now increases the test variable by 2
;; each time, starting from 3, and the prime? procedure checks if n is odd
;; before starting the algorithm. We observe a speed improvement by about a
;; factor of 2. Additionally, we can improve the search-for-primes? procedure by
;; similarly ignoring even numbers.
