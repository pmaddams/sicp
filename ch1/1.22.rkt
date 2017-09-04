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
;; 1009 *** 37
;; 1013 *** 38
;; 1019 *** 39

(search-for-primes 10000 10040)
;; 10007 *** 116
;; 10009 *** 126
;; 10037 *** 117

(search-for-primes 100000 100050)
;; 100003 *** 346
;; 100019 *** 345
;; 100043 *** 346

(search-for-primes 1000000 1000040)
;; 1000003 *** 1086
;; 1000033 *** 1086
;; 1000037 *** 1086

(define (ratios-to-sqrt l)
  (for-each displayln
            (map (lambda (a b)
                   (/ a (sqrt b)))
                 (map timed-prime-test l)
                 l)))

(ratios-to-sqrt '(1009 10007 100003 1000003))
;; 1009 *** 55
;; 10007 *** 140
;; 100003 *** 414
;; 1000003 *** 1286
;; 1.7314785125565362
;; 1.3995102571000293
;; 1.3091633140072776
;; 1.2859980710043402

;; The timing data supports the idea that runtime is proportional to the number
;; of steps required, especially for a large number of steps. Given that the
;; algorithm has order of growth theta(sqrt(n)), the ratio of runtime to sqrt(n)
;; appears to converge as n approaches infinity.
