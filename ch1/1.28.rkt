#lang sicp

(define (expmod-with-composite-test a n m)
  (let ((square (lambda (x)
                  (expt x 2))))
    (letrec ((e (lambda (n)
                  (cond ((zero? n) 1)
                        ((even? n) (let* ((x (e (/ n 2)))
                                          (y (remainder (square x) m)))
                                     (if (and (= y 1)
                                              (not (= x 1))
                                              (not (= x (dec m))))
                                         0
                                         y)))
                        (else (remainder (* a (e (dec n))) m))))))
      (e n))))

(define (miller-rabin n)
  (let ((a (inc (random (dec n)))))
    (= 1 (expmod-with-composite-test a (dec n) n))))

(define (prime? n)
  (letrec ((p (lambda (times)
                (or (zero? times)
                    (and (miller-rabin n)
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
;; 1009 *** 199
;; 1013 *** 204
;; 1019 *** 208

(search-for-primes 10000 10040)
;; 10007 *** 247
;; 10009 *** 257
;; 10037 *** 248

(search-for-primes 100000 100050)
;; 100003 *** 288
;; 100019 *** 291
;; 100043 *** 292

(search-for-primes 1000000 1000040)
;; 1000003 *** 333
;; 1000033 *** 345
;; 1000037 *** 351

(let ((carmichael '(561 1105 1729 2465 2821 6601)))
  (for-each (lambda (n)
              (display n)
              (display (if (prime? n)
                           " fools "
                           " doesn't fool "))
              (display "the Miller-Rabin test.")
              (newline))
            carmichael))
;; 561 doesn't fool the Miller-Rabin test.
;; 1105 doesn't fool the Miller-Rabin test.
;; 1729 doesn't fool the Miller-Rabin test.
;; 2465 doesn't fool the Miller-Rabin test.
;; 2821 doesn't fool the Miller-Rabin test.
;; 6601 doesn't fool the Miller-Rabin test.
