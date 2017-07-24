#lang sicp

(define (prime? n)
  (letrec ((square (lambda (x)
                     (expt x 2)))
           (divides? (lambda (a b)
                       (zero? (remainder b a))))
           (next (lambda (test-divisor)
                   (+ test-divisor 2)))
           (find-divisor (lambda (test-divisor)
                           (cond ((> (square test-divisor) n) n)
                                 ((divides? test-divisor n) test-divisor)
                                 (else (find-divisor (next test-divisor))))))
           (smallest-divisor (lambda ()
                               (find-divisor 3))))
    (cond ((<= n 1) #f)
          ((= n 2) #t)
          ((even? n) #f)
          (else (= n (smallest-divisor))))))

(define (timed-prime-test n)
  (let* ((report-prime (lambda (elapsed-time)
                         (begin (display " *** ")
                                 (display elapsed-time))))
         (start-prime-test (lambda (start-time)
                             (if (prime? n)
                                 (report-prime (- (runtime) start-time))))))
    (begin (newline)
           (display n)
           (start-prime-test (runtime)))))

(define (search-for-primes n bound)
  (if (< n bound)
      (if (odd? n)
          (begin (timed-prime-test n)
                 (search-for-primes (+ n 2) bound))
          (search-for-primes (inc n) bound))
      (newline)))

(search-for-primes 1000 1020)
;; 1009 *** 31
;; 1013 *** 33
;; 1019 *** 32

(search-for-primes 10000 10040)
;; 10007 *** 68
;; 10009 *** 69
;; 10037 *** 67

(search-for-primes 100000 100050)
;; 100003 *** 182
;; 100019 *** 183
;; 100043 *** 182

(search-for-primes 1000000 1000040)
;; 1000003 *** 546
;; 1000033 *** 545
;; 1000037 *** 545

;; The improved next procedure increases n by 2 each time without checking its
;; value, because smallest-divisor now starts at 3, and the prime? procedure
;; first checks if n is even before starting the algorithm. The speed
;; improvement is about a factor of 2, especially for large numbers. This
;; supports the previous conclusion that runtime is dependent on order of growth
;; for large values of n.
