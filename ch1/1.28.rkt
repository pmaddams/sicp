#lang racket/base

(require racket/generator)

(module+ test
  (require rackunit))

(define (make-prime? f)
  (lambda (n)
    (if (< n 2)
        #f
        (f n))))

(define (trial-division n)
  (define (loop i)
    (cond ((= n i) #t)
          ((zero? (modulo n i)) #f)
          (else (loop (add1 i)))))
  (loop 2))

(define (fermat k)
  (lambda (n)
    ()))

(define (square n)
  (* n n))

(define (expmod b x m)
  (define (loop b x)
    (cond ((zero? x) 1)
          ((even? x) (modulo (square (loop b (/ x 2))) m))
          (else (modulo (* b (loop b (sub1 x))) m))))
  (loop b x))

(module+ test
  (for ((i (in-range 10)))
    (let ((b (random 100))
          (x (random 100))
          (m (random 100)))
      (check = (expmod b x m) (modulo (expt b x) m)))))

(define (miller-rabin k)
  (lambda (n)
    ()))
