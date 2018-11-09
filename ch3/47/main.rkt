#lang racket/base

; Exercise 3.47: Semaphores

(require ffi/unsafe/atomic
         racket/class)

(define-syntax-rule (concurrent expr ...)
  (for-each thread (list (lambda () expr) ...)))

(define cell%
  (class object%
    (super-new)
    (field (val #f))

    (define/public (test-and-set)
      (call-as-atomic
       (lambda ()
         (let ((old-val val))
           (set! val #t)
           old-val))))

    (define/public (clear)
      (set! val #f))))

(define (test-and-set cell)
  (send cell test-and-set))

(define (clear cell)
  (send cell clear))

(define mutex%
  (class cell%
    (super-new)

    (define/public (lock)
      (when (test-and-set this) (lock)))

    (define/public (unlock)
      (clear this))))

(define (lock mutex)
  (send mutex lock))

(define (unlock mutex)
  (send mutex unlock))
