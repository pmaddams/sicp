#lang racket/base

(provide make-prime?
         trial-division
         expmod
         fermat
         miller-rabin
         fools-fermat?
         fools-miller-rabin?)

(require racket/dict)

(define (search-for-primes from to)
  (for ((n (in-range from (add1 to))))
    (timed-prime-test (miller-rabin 10) n)))

(define (timed-prime-test with-prime? n)
  (define (now) (current-inexact-milliseconds))
  (let ((start (now)))
    (when (with-prime? n)
      (printf "~a (~a ms)\n" n (- (now) start)))))

(define (make-prime? f)
  (lambda (n)
    (and (not (< n 2))
         (or (<= n 3)
             (f n)))))

(define (trial-division n)
  (let loop ((i 2))
    (or (= i n)
        (and (not (zero? (modulo n i)))
             (loop (add1 i))))))

(define (probable-prime with-expmod)
  (lambda (k)
    (lambda (n)
      (let loop ((k k))
        (or (zero? k)
            (let ((a (random 2 (sub1 n))))
              (and (= (with-expmod a (sub1 n) n) 1)
                   (loop (sub1 k)))))))))

(define (expmod b x m)
  (let loop ((x x))
    (cond ((zero? x) 1)
          ((even? x) (modulo (square (loop (/ x 2))) m))
          (else (modulo (* b (loop (sub1 x))) m)))))

(define (square n)
  (* n n))

(define fermat (probable-prime expmod))

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

(define miller-rabin (probable-prime expmod-nontrivial-sqrt))

(define (fools-prime? with-expmod)
  (lambda (n)
    (let loop ((a (sub1 n)))
      (or (< a 2)
          (and (= (with-expmod a n n) a)
               (loop (sub1 a)))))))

(define fools-fermat? (fools-prime? expmod))

(define fools-miller-rabin? (fools-prime? expmod-nontrivial-sqrt))
