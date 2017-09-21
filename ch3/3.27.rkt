#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-has-key?
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (k)
                (hash-ref table k #f)))
         (put (lambda (k v)
                (hash-set! table k v)))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table))

(define get (table 'get))

(define put (table 'put))

(define (memoize f)
  (lambda (x)
    (or (get x)
        (let ((result (f x)))
          (put x result)
          result))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((zero? n) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (dec n))
                            (memo-fib (- n 2))))))))

(memo-fib 10)
;; 55

(memo-fib 1000)
;; 43466557686937456435688527675040625802564660517371780402481729089536555417949
;; 05189040387984007925516929592259308032263477520968962323987332247116164299644
;; 0906533187938298969649928516003704476137795166849228875

;; (memoize fib) would not work, because fib makes recursive calls to itself and
;; not memo-fib.
