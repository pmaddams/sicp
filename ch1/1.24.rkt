#lang sicp

(define (expmod a n m)
  (let ((square (lambda (x)
                  (expt x 2))))
    (letrec ((e (lambda (n)
                  (cond ((zero? n) 1)
                        ((even? n) (remainder (square (e (/ n 2))) m))
                        (else (remainder (* a (e (dec n))) m))))))
      (e n))))

(define (fermat-test n)
  (let ((a (inc (random (dec n)))))
    (= a (expmod a n n))))

(define (prime? n)
  (letrec ((p (lambda (times)
                (or (zero? times)
                    (and (fermat-test n)
                         (p (dec times)))))))
    (and (> n 1)
         (not (= n 2))
         (odd? n)
         (p 10))))

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
;; 1009 *** 205
;; 1013 *** 213
;; 1019 *** 224

(search-for-primes 10000 10040)
;; 10007 *** 267
;; 10009 *** 292
;; 10037 *** 263

(search-for-primes 100000 100050)
;; 100003 *** 298
;; 100019 *** 306
;; 100043 *** 302

(search-for-primes 1000000 1000040)
;; 1000003 *** 358
;; 1000033 *** 342
;; 1000037 *** 351

;; Since the Fermat test has theta(log(n)) growth, the running time should
;; increase linearly as n increases by orders of magnitude. The data supports
;; this hypothesis.
