#lang racket/base

; Exercise 1.28

(provide trial-division
         expmod
         fermat
         miller-rabin
         fools-fermat
         fools-miller-rabin)

(define (search-for-primes lo hi)
  (for ((n (in-range lo (add1 hi))))
    (timed-prime-test (miller-rabin 10) n)))

(define (timed-prime-test p n)
  (define (now) (current-inexact-milliseconds))
  (let ((start (now)))
    (when (p n)
      (printf "~a (~a ms)\n" n (- (now) start)))))

(define ((make-prime p) n)
  (and (not (< n 2))
       (or (<= n 3)
           (p n))))

(define trial-division
  (make-prime
   (lambda (n)
     (for/fold ((prime #t))
               ((d (in-range 2 (add1 (integer-sqrt n)))))
       (and (not (zero? (remainder n d))) prime)))))

(define ((probable-prime expmod) k)
  (make-prime
   (lambda (n)
     (for/fold ((prime #t))
               ((i (in-range k)))
       (let ((a (random 2 (sub1 n))))
         (and (= 1 (expmod a (sub1 n) n)) prime))))))

(define (expmod a n m)
  (let loop ((n n))
    (cond ((zero? n) 1)
          ((even? n)
           (remainder (square (loop (quotient n 2))) m))
          (else
           (remainder (* a (loop (sub1 n))) m)))))

(define-syntax-rule (neither expr ...)
  (not (or expr ...)))

(define (expmod-nontrivial-sqrt a n m)
  (let loop ((n n))
    (cond ((zero? n) 1)
          ((even? n)
           (let* ((r (loop (quotient n 2)))
                  (x (remainder (square r) m)))
             (if (and (neither (= r 1) (= r (sub1 m)))
                      (= x 1))
                 0
                 x)))
          (else
           (remainder (* a (loop (sub1 n))) m)))))

(define fermat (probable-prime expmod))

(define miller-rabin (probable-prime expmod-nontrivial-sqrt))

(define (fools expmod)
  (lambda (n)
    (let loop ((a (sub1 n)))
      (or (< a 2)
          (and (= a (expmod a n n))
               (loop (sub1 a)))))))

(define fools-fermat (fools expmod))

(define fools-miller-rabin (fools expmod-nontrivial-sqrt))

(define (square n) (* n n))
