#lang sicp

(#%require racket/base)
(#%require ffi/unsafe/atomic)

(define (test-and-set! cell)
  (start-atomic)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f))
  (end-atomic))

(define (clear! cell)
  (set-car! cell #f))

(define (make-mutex)
  (let* ((cell '(#f))
         (the-mutex (lambda (m)
                      (cond ((eq? m 'acquire)
                             (if (test-and-set! cell)
                                 (the-mutex 'acquire)))
                            ((eq? m 'release)
                             (clear! cell))))))
    the-mutex))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (letrec ((p (lambda args
                    (mutex 'acquire)
                    (let ((val (apply p args)))
                      (mutex 'release)
                      val))))
        serialized-p))))

(define (parallel-execute . args)
  (for-each thread args))
