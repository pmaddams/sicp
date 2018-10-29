#lang racket/base

(provide prime?)

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
