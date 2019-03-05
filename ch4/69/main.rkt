#lang racket/base

; Exercise 4.69

(provide (all-defined-out))

(require racket/promise
         racket/stream)

(struct expr (type val))

(define table (make-hash))

(define (put k v) (hash-set! table k v))

(define (get k) (hash-ref table k))

(define (type expr)
  (if (pair? expr)
      (car expr)
      (error "syntax error:" expr)))

(define (body expr)
  (if (pair? expr)
      (cdr expr)
      (error "syntax error:" expr)))

(define (instantiate expr frame handler)
  (let loop ((expr expr))
    (cond ((variable? expr)
           (let ((binding (assoc expr frame)))
             (if binding
                 (let ((val (cdr binding)))
                   (loop val))
                 (handler expr frame))))
          ((pair? expr)
           (cons (loop (car expr))
                 (loop (cdr expr))))
          (else expr))))

(define (eval query s)
  (let ((proc (get (cons (type query) 'eval))))
    (if proc
        (proc (body query) s)
        (ask query s))))

(define (variable? expr)
  (tagged-list? expr '?))

(define (rule? stmt)
  (tagged-list? stmt 'rule))

(define (tagged-list? x tag)
  (and (pair? x)
       (eq? tag (car x))))

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
