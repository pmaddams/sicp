#lang sicp

(define (smallest-divisor n)
  (let ((square (lambda (x)
                  (expt x 2)))
        (divides? (lambda (a b)
                    (zero? (remainder b a)))))
    (letrec ((s (lambda (i)
                  (cond ((> (square i) n) n)
                        ((divides? i n) i)
                        (else (s (inc i)))))))
      (s 2))))

(define (prime? n)
  (and (> n 1)
       (not (= n 2))
       (odd? n)
       (= n (smallest-divisor n))))

(define (displayln x)
  (display x)
  (newline))

(define (timed-prime-test n)
  (let ((start (runtime)))
    (if (prime? n)
        (let ((elapsed (- (runtime) start)))
          (display n)
          (display " *** ")
          (displayln elapsed)
          elapsed))))

(define (search-for-primes n lim)
  (letrec ((s (lambda (n)
                (if (< n lim)
                    (begin (timed-prime-test n)
                           (s (inc n)))))))
    (s n)))

(search-for-primes 1000 1020)
;; 1009 *** 42
;; 1013 *** 42
;; 1019 *** 42

(search-for-primes 10000 10040)
;; 10007 *** 125
;; 10009 *** 129
;; 10037 *** 124

(search-for-primes 100000 100050)
;; 100003 *** 386
;; 100019 *** 385
;; 100043 *** 385

(search-for-primes 1000000 1000040)
;; 1000003 *** 1214
;; 1000033 *** 1282
;; 1000037 *** 1212

(define (ratios-to-sqrt l)
  (for-each displayln
            (map (lambda (a b)
                   (/ a (sqrt b)))
                 (map timed-prime-test l)
                 l)))

(ratios-to-sqrt '(1009 10007 100003 1000003))
;; 1009 *** 52
;; 10007 *** 134
;; 100003 *** 394
;; 1000003 *** 1219
;; 1.6370342300534524
;; 1.3395312460814568
;; 1.245918709465863
;; 1.218998171504114

;; The timing data supports the idea that runtime is proportional to the number
;; of steps required, especially for a large number of steps. Given that the
;; algorithm has order of growth theta(sqrt(n)), the ratio of runtime to sqrt(n)
;; appears to converge as n approaches infinity.
