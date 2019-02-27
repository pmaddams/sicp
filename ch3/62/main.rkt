#lang racket/base

; Exercise 3.62

(require racket/stream)

(define (add s1 s2)
  (zip-with + s1 s2))

(define (sub s1 s2)
  (zip-with - s1 s2))

(define (mul s1 s2)
  (let ((a (stream-first s1))
        (b (stream-first s2)))
    (stream-cons (* a b)
                 (add (mul (stream-rest s1) s2)
                      (mul (series a) (stream-rest s2))))))

(define (div s1 s2)
  (let ((a (stream-first s1))
        (b (stream-first s2)))
    (letrec ((q (stream-cons (/ a b)
                             (mul (series (/ 1 b))
                                  (sub (stream-rest s1)
                                       (mul q (stream-rest s2)))))))
      q)))

(define sin
  (stream-cons 0 (integral cos)))

(define cos
  (stream-cons 1 (negative (integral sin))))

(define tan (div sin cos))

(define exp (stream-cons 1 (integral exp)))

(define (integral s)
  (zip-with / s (in-naturals 1)))

(define (negative s)
  (stream-map (lambda (n) (- n)) s))

(define (series n)
  (stream-cons n (let loop ()
                   (stream-cons 0 (loop)))))

(define (zip-with f . args)
  (let loop ((l args))
    (if (ormap stream-empty? l)
        empty-stream
        (stream-cons (apply f (map stream-first l))
                     (loop (map stream-rest l))))))
