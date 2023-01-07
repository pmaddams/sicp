#lang racket/base

; Exercise 1.28

(provide (all-defined-out))

(define (search-for-primes lo hi)
  (let ((p (miller-rabin 10)))
    (for ((n (in-range lo (add1 hi))))
      (timed-prime-test p n))))

(define (timed-prime-test p n)
  (define (now) (current-inexact-milliseconds))
  (let ((start (now)))
    (when (p n)
      (printf "~a (~a ms)\n" n (- (now) start)))))

(define ((make-prime p) n)
  (and (not (< n 2))
       (or (<= n 3)
           (and (odd? n)
                (p n)))))

(define trial-division
  (make-prime
   (lambda (n)
     (for/fold ((acc #t))
               ((d (in-range 2 (add1 (integer-sqrt n)))))
       (and acc (not (zero? (remainder n d))))))))

(define ((probable-prime mod-expt) k)
  (make-prime
   (lambda (n)
     (for/fold ((acc #t))
               ((i (in-range k)))
       (let ((b (random 2 (sub1 n))))
         (and acc (= 1 (mod-expt b (sub1 n) n))))))))

(define (mod-expt b n m)
  (let loop ((n n))
    (cond ((zero? n) 1)
          ((even? n)
           (remainder (square (loop (quotient n 2))) m))
          (else
           (remainder (* b (loop (sub1 n))) m)))))

(define-syntax-rule (neither expr ...)
  (not (or expr ...)))

(define (mod-expt-nontrivial-sqrt b n m)
  (let loop ((n n))
    (cond ((zero? n) 1)
          ((even? n)
           (let* ((root (loop (quotient n 2)))
                  (rem (remainder (square root) m)))
             (if (and (neither (= root 1) (= root (sub1 m)))
                      (= rem 1))
                 0
                 rem)))
          (else
           (remainder (* b (loop (sub1 n))) m)))))

(define fermat (probable-prime mod-expt))

(define miller-rabin (probable-prime mod-expt-nontrivial-sqrt))

(define (fools mod-expt)
  (lambda (n)
    (let loop ((b (sub1 n)))
      (or (< b 2)
          (and (= b (mod-expt b n n))
               (loop (sub1 b)))))))

(define fools-fermat (fools mod-expt))

(define fools-miller-rabin (fools mod-expt-nontrivial-sqrt))

(define (square n) (* n n))
