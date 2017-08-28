#lang sicp

(define stream-car car)

(define (stream-cdr s)
  (force (cdr s)))

(define stream-null? null?)

(define the-empty-stream '())

(define (stream-ref s n)
  (if (zero? n)
      (stream-car s)
      (stream-ref (stream-cdr s) (dec n))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (inc low) high))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc . argstreams)
  (if (not (stream-null? (car argstreams)))
      (begin (apply proc (map stream-car argstreams))
             (apply stream-for-each
                    (cons proc (map stream-cdr argstreams))))))
