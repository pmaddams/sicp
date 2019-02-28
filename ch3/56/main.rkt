#lang racket/base

; Exercise 3.56

(provide (all-defined-out))

(require racket/stream)

(define hamming
  (stream-cons 1 (merge (scale hamming 2)
                        (scale hamming 3)
                        (scale hamming 5))))

(define (merge . args)
  (letrec ((loop (lambda (s1 s2)
                   (cond ((stream-empty? s1) s2)
                         ((stream-empty? s2) s1)
                         (else
                          (let ((a (stream-first s1))
                                (b (stream-first s2)))
                            (cond ((< a b)
                                   (stream-cons a (loop (stream-rest s1)
                                                        s2)))
                                  ((> a b)
                                   (stream-cons b (loop s1
                                                        (stream-rest s2))))
                                  (else
                                   (stream-cons a (loop (stream-rest s1)
                                                        (stream-rest s2)))))))))))
    (foldr loop empty-stream args)))

(define (scale s n)
  (for/stream ((i s))
    (* n i)))
