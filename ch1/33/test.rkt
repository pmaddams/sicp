#lang racket/base

(require rackunit
         "main.rkt")

(define primes '(2 3 5 7 11 13 17 23 29 31))

(define composites '(0 1 4 6 8 9 10 12 14 15))

(define (range lo hi)
  (for/list ((n (in-range lo (add1 hi)))) n))

(test-case
 "sum-squares-prime"
 (for ((i (in-range 5)))
   (let* ((lo (random 10 100))
          (hi (+ lo (random 10 100))))
     (check-equal? (sum-squares-prime lo hi)
                   (apply + (map square (filter prime? (range lo hi))))))))

(test-case
 "product-relative-prime-upto"
 (for ((i (in-range 5)))
   (let ((n (random 10 100)))
     (check-equal? (product-relative-prime-upto n)
                   (apply * (filter (lambda (i) (= (gcd i n) 1)) (range 2 n)))))))

(test-case
 "prime?"
 (for ((n primes))
   (check-true (prime? n)))
 (for ((n composites))
   (check-false (prime? n))))
