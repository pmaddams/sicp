#lang racket/base

(module+ test
  (require rackunit))

(define (make-prime? f)
  (lambda (n)
    (and (not (< n 2))
         (or (<= n 3)
             (f n)))))

(module+ test
  (define (check-prime? f)
    (let ((prime? (make-prime? f)))
      (for ((p '(2 3 5 7 11 13)))
        (check-true (prime? p)))
      (for ((c '(0 1 4 6 8 9)))
        (check-false (prime? c))))))

(define (trial-division n)
  (let loop ((i 2))
    (cond ((= n i) #t)
          ((zero? (modulo n i)) #f)
          (else (loop (add1 i))))))

(module+ test
  (check-prime? trial-division))

(define (probable-prime with-expmod)
  (lambda (k)
    (lambda (n)
      (let loop ((k k))
        (or (zero? k)
            (let ((a (random 2 (sub1 n))))
              (and (= 1 (with-expmod a (sub1 n) n))
                   (loop (sub1 k)))))))))

(define (expmod b x m)
  (let loop ((x x))
    (cond ((zero? x) 1)
          ((even? x) (modulo (square (loop (/ x 2))) m))
          (else (modulo (* b (loop (sub1 x))) m)))))

(define (square n)
  (* n n))

(module+ test
  (for ((i (in-range 10)))
    (let ((b (random 2 100))
          (x (random 2 100))
          (m (random 2 100)))
      (check-eq? (expmod b x m)
                 (modulo (expt b x) m)))))

(define fermat (probable-prime expmod))

(module+ test
  (check-prime? (fermat 10)))

(define (miller-expmod b x m)
  (let loop ((x x))
    (cond ((zero? x) 1)
          ((even? x) (let* ((n (loop (/ x 2)))
                            (r (modulo (square n) m)))
                       (if (and (not (or (= n 1)
                                         (= n (sub1 m))))
                                (= r 1))
                           0
                           r)))
          (else (modulo (* b (loop (sub1 x))) m)))))

(define miller-rabin (probable-prime miller-expmod))

(module+ test
  (check-prime? (miller-rabin 10)))

(define (fools-prime? with-expmod)
  (lambda (n)
    (let loop ((i (sub1 n)))
      (or (< i 2)
          (and (= i (with-expmod i n n))
               (loop (sub1 i)))))))

(define fools-fermat? (fools-prime? expmod))

(define fools-miller-rabin? (fools-prime? miller-expmod))

(module+ test
  (for ((carmichael '(561 1105 1729 2465 2821 6601)))
    (check-true (fools-fermat? carmichael))
    (check-false (fools-miller-rabin? carmichael))))

(define (search-for-primes with-prime? seq)
  (define (now) (current-inexact-milliseconds))
  (define (timed-prime-test n)
    (let ((start (now)))
      (when (with-prime? n)
        (printf "~a is prime: ~a ms\n" n (- (now) start)))))
  (for ((n seq))
    (timed-prime-test n)))

(module+ main
  (require racket/dict)
  (for (((name f)
         (in-dict `(("trial division" . ,trial-division)
                    ("fermat" . ,(fermat 10))
                    ("miller-rabin" . ,(miller-rabin 10))))))
    (printf "~a:\n" name)
    (search-for-primes (make-prime? f)
                       (in-range 100000 100100))))
