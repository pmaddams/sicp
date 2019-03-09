#lang racket/base

; Exercise 3.56

(provide (all-defined-out))

(require racket/stream)

(define hamming
  (stream-cons 1 (merge (scale hamming 2)
                        (scale hamming 3)
                        (scale hamming 5))))

(define (merge . args)
  (letrec ((loop (lambda (st1 st2)
                   (cond ((stream-empty? st1) st2)
                         ((stream-empty? st2) st1)
                         (else
                          (let ((n (stream-first st1))
                                (m (stream-first st2)))
                            (cond ((< n m)
                                   (stream-cons n (loop (stream-rest st1)
                                                        st2)))
                                  ((> n m)
                                   (stream-cons m (loop st1
                                                        (stream-rest st2))))
                                  (else
                                   (stream-cons n (loop (stream-rest st1)
                                                        (stream-rest st2)))))))))))
    (foldr loop empty-stream args)))

(define (scale st n)
  (for/stream ((i st))
    (* n i)))
