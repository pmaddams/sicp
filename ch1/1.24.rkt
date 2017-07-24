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
  (cond ((<= n 1) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (fast-prime? n 10))))

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
;; 1009 *** 197
;; 1013 *** 200
;; 1019 *** 208

(search-for-primes 10000 10040)
;; 10007 *** 246
;; 10009 *** 240
;; 10037 *** 244

(search-for-primes 100000 100050)
;; 100003 *** 281
;; 100019 *** 285
;; 100043 *** 289

(search-for-primes 1000000 1000040)
;; 1000003 *** 322
;; 1000033 *** 320
;; 1000037 *** 330

;; Since the Fermat test has theta of log(n) growth, the runtime should increase
;; linearly as n increases by orders of magnitude. The data supports this
;; hypothesis.
