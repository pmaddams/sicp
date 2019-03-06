#lang racket/base

; Exercise 4.69

(provide (all-defined-out))

(require racket/promise
         racket/stream)

(struct database (assertions rules) #:mutable)

(define db (database empty-stream empty-stream))

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

(define (instantiate expr frame fail)
  (let copy ((expr expr))
    (cond ((variable? expr)
           (let ((binding (assoc expr frame)))
             (if binding
                 (copy (cdr binding))
                 (fail expr frame))))
          ((pair? expr)
           (cons (copy (car expr))
                 (copy (cdr expr))))
          (else expr))))

(define (eval query s)
  (let ((proc (get (cons (type query) 'eval))))
    (if proc
        (proc (body query) s)
        (simple-query query s))))

(define (simple-query query s)
  (stream-append-map
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query frame)
      (delay (apply-rules query frame))))
   s))

(define (and-query clauses s)
  (if (null? clauses)
      s
      (and-query (cdr clauses)
               (eval (car clauses) s))))

(define (or-query clauses s)
  (if (null? clauses)
      empty-stream
      (interleave-delayed
       (eval (car clauses) s)
       (delay (or-query (cdr clauses) s)))))

(define (match pattern datum frame)
  (cond ((void? frame) (void))
        ((equal? pattern datum) frame)
        ((var? pattern) (extend-if-consistent pattern datum frame))
        ((and (pair? pattern)
              (pair? datum))
         (let ((frame* (match (car pattern) (car datum) frame)))
           (match (cdr pattern) (cdr datum) frame*)))
        (else (void))))

(define (extend-if-consistent var datum frame)
  (let ((binding (assoc var frame)))
    (if binding
        (match (cdr binding) datum frame)
        (extend var datum frame))))

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
