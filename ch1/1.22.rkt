#lang sicp

(define (prime? n)
  (letrec ((square (lambda (x)
                     (expt x 2)))
           (divides? (lambda (a b)
                       (zero? (remainder b a))))
           (find-divisor (lambda (test-divisor)
                           (cond ((> (square test-divisor) n) n)
                                 ((divides? test-divisor n) test-divisor)
                                 (else (find-divisor (inc test-divisor))))))
           (smallest-divisor (lambda ()
                               (find-divisor 2))))
    (if (<= n 1)
        #f
        (= n (smallest-divisor)))))

(define (timed-prime-test n)
  (let* ((report-prime (lambda (elapsed-time)
                         (display " *** ")
                         (display elapsed-time)))
         (start-prime-test (lambda (start-time)
                             (if (prime? n)
                                 (report-prime (- (runtime) start-time))))))
    (newline)
    (display n)
    (start-prime-test (runtime))))

(define (search-for-primes n bound)
  (if (< n bound)
      (if (odd? n)
          (begin (timed-prime-test n)
                 (search-for-primes (+ n 2) bound))
          (search-for-primes (inc n) bound))
      (newline)))

(search-for-primes 1000 1020)
;; 1009 *** 51
;; 1013 *** 50
;; 1019 *** 50

(search-for-primes 10000 10040)
;; 10007 *** 119
;; 10009 *** 119
;; 10037 *** 118

(search-for-primes 100000 100050)
;; 100003 *** 350
;; 100019 *** 354
;; 100043 *** 349

(search-for-primes 1000000 1000040)
;; 1000003 *** 1142
;; 1000033 *** 1142
;; 1000037 *** 1140

(/ 51. (sqrt 1000))
;; 1.6127616066858734

(/ 119. (sqrt 10000))
;; 1.19

(/ 350. (sqrt 100000))
;; 1.1067971810589328

(/ 1142. (sqrt 1000000))
;; 1.142

;; The timing data supports the idea that runtime is proportional to the number
;; of steps required, especially for a large number of steps. Given that the
;; algorithm has order of growth theta of sqrt(n), the ratio of runtime to
;; sqrt(n) appears to approach a constant value as n approaches infinity.
