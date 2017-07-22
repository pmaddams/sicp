#lang sicp

(define (expmod base exp m)
  (letrec ((square (lambda (x)
                     (expt x 2)))
           (e (lambda (exp)
                (cond ((zero? exp) 1)
                      ((even? exp) (remainder (square (e (/ exp 2)))
                                              m))
                      (else (remainder (* base (e (dec exp)))
                                       m))))))
    (e exp)))

(define (fast-prime? n times)
  (letrec ((fermat-test (lambda (n)
                          (let ((a (inc (random (dec n)))))
                            (= a (expmod a n n)))))
           (f (lambda (times)
                (cond ((zero? times) #t)
                      ((fermat-test n) (f (dec times)))
                      (else #f)))))
    (f times)))

(define (prime? n)
  (if (or (<= n 1)
            (even? n))
        #f
        (fast-prime? n 10)))

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
;; 1009 *** 203
;; 1013 *** 205
;; 1019 *** 210

(search-for-primes 10000 10040)
;; 10007 *** 250
;; 10009 *** 240
;; 10037 *** 246

(search-for-primes 100000 100050)
;; 100003 *** 280
;; 100019 *** 284
;; 100043 *** 285

(search-for-primes 1000000 1000040)
;; 1000003 *** 321
;; 1000033 *** 322
;; 1000037 *** 326

;; Since the Fermat test has theta of log(n) growth, the runtime should increase
;; linearly as n increases by orders of magnitude. The data supports this
;; hypothesis.
