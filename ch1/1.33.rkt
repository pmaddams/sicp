#lang sicp

(define (filtered-accumulate test? combiner null-value term a next b)
  (letrec ((p (lambda (a result)
                (cond ((> a b) result)
                      ((test? a) (p (next a)
                                    (combiner result (term a))))
                      (else (p (next a)
                               result))))))
    (p a null-value)))

(define (square x)
  (expt x 2))

(define (expmod-with-composite-test a n m)
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
    (e n)))

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

(define (sum-of-squared-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(sum-of-squared-primes 1 100)
;; 65792

(define (product-relatively-prime-less-than n)
  (letrec ((gcd (lambda (a b)
                  (if (zero? b)
                      a
                      (gcd b (remainder a b)))))
           (test? (lambda (x)
                    (= 1 (gcd x n)))))
    (filtered-accumulate test? * 1 identity 2 inc (dec n))))

(product-relatively-prime-less-than 100)
;; 426252881942771063138176712755660145456313428952105524817872601
