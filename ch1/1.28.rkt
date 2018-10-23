#lang racket/base

(module+ test
  (require rackunit))

(define (make-prime? f)
  (lambda (n)
    (and (not (< n 2))
         (or (<= n 3)
             (f n)))))

(define (trial-division n)
  (let loop ((i 2))
    (cond ((= n i) #t)
          ((zero? (modulo n i)) #f)
          (else (loop (add1 i))))))

(define (fermat k)
  (lambda (n)
    (let loop ((k k))
      (or (zero? k)
          (let ((a (random 2 (sub1 n))))
            (and (= 1 (expmod a (sub1 n) n))
                 (loop (sub1 k))))))))

(define (expmod b x m)
  (let loop ((x x))
    (cond ((zero? x) 1)
          ((even? x) (modulo (square (loop (/ x 2))) m))
          (else (modulo (* b (loop (sub1 x))) m)))))

(define (square n)
  (* n n))

(module+ test
  (for ((i (in-range 10)))
    (let ((b (random 100))
          (x (random 100))
          (m (random 100)))
      (check = (expmod b x m) (modulo (expt b x) m)))))

#;(define (miller-expmod b x m)
    (define (loop b x)
      (cond ((zero? x) 1)
            ((even? x) (let ((x (modulo (square (loop b (/ x 2))) m)))
                         (and (not ())
                              (not ())
                              x)))
            (else (modulo (* b (loop b (sub1 x))) m))))
    (loop b x))

#;(define (miller-rabin k)
    (lambda (n)
      (define (loop k)
        (or (zero? k)
            (let ((a (random 2 (sub1 n))))
              ())))
      (loop k)))

(module+ test
  (let ((prime? (make-prime? (fermat 10))))
    (for ((n '(2 3 5 7 11 13)))
      (check-true (prime? n)))))
