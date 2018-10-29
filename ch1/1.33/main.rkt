#lang racket/base

(provide sum-squares-prime
         product-relative-prime-upto
         prime?
         square)

(require racket/function)

(define (sum-squares-prime lo hi)
  (filtered-accumulate prime? + 0 square lo add1 hi))

(define (product-relative-prime-upto n)
  (filtered-accumulate (lambda (i) (= (gcd i n) 1)) * 1 identity 1 add1 n))

(define (filtered-accumulate pred combine z term lo next hi)
  (let loop ((n lo) (acc z))
    (cond ((> n hi) acc)
          ((pred n) (loop (next n) (combine (term n) acc)))
          (else (loop (next n) acc)))))

(define (prime? n)
  (and (not (< n 2))
       (or (<= n 3)
           ((miller-rabin 10) n))))

(define (miller-rabin k)
  (lambda (n)
    (let loop ((k k))
      (or (zero? k)
          (let ((a (random 2 (sub1 n))))
            (and (= (expmod-nontrivial-sqrt a (sub1 n) n) 1)
                 (loop (sub1 k))))))))

(define-syntax-rule (neither expr ...)
  (not (or expr ...)))

(define (expmod-nontrivial-sqrt b x m)
  (let loop ((x x))
    (cond ((zero? x) 1)
          ((even? x) (let* ((n (loop (/ x 2)))
                            (r (modulo (square n) m)))
                       (if (and (neither (= n 1) (= n (sub1 m)))
                                (= r 1))
                           0
                           r)))
          (else (modulo (* b (loop (sub1 x))) m)))))

(define (square n)
  (* n n))
