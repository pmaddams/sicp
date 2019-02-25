#lang racket/base

; Exercise 3.47

(provide (all-defined-out))

(require ffi/unsafe/atomic
         racket/class
         racket/function)

(define-syntax-rule (concurrent expr ...)
  (for-each thread (list (thunk expr) ...)))

(define cell%
  (class object%
    (super-new)

    (field (val #f))

    (define/public (test-and-set)
      (call-as-atomic
       (thunk
        (let ((old-val val))
          (set! val #t)
          old-val))))

    (define/public (clear)
      (set! val #f))))

(define semaphore%
  (class cell%
    (super-new)

    (init-field size)
    (field (count 0))

    (define/public (acquire)
      (if (or (>= count size)
              (send this test-and-set))
          (acquire)
          (begin (set! count (add1 count))
                 (send this clear))))

    (define/public (release)
      (unless (zero? count)
        (if (send this test-and-set)
            (release)
            (begin (set! count (sub1 count))
                   (send this clear)))))))

(define mutex%
  (class semaphore%
    (super-new (size 1))))

(define (acquire x) (send x acquire))

(define (release x) (send x release))
