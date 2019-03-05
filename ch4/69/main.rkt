#lang racket/base

; Exercise 4.69

(provide (all-defined-out))

(require racket/promise
         racket/stream)

(struct expr (type val))

(define table (make-hash))

(define (put k v) (hash-set! table k v))

(define (get k) (hash-ref table k))

(define (stream-append-map f s)
  (stream-flatten (stream-map f s)))

(define (stream-flatten s)
  (if (stream-empty? s)
      empty-stream
      (interleave-delayed
       (stream-first s)
       (delay (stream-flatten (stream-rest s))))))

(define (interleave-delayed s ds)
  (if (stream-empty? s)
      (force ds)
      (stream-cons (stream-first s)
                   (interleave-delayed
                    (force ds)
                    (delay (stream-rest s))))))

(define (stream-append-delayed s ds)
  (if (stream-empty? s)
      (force ds)
      (stream-cons (stream-first s)
                   (stream-append-delayed
                    (stream-rest s)
                    ds))))
