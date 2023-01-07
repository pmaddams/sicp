#lang racket/base

; Exercise 2.42

(provide (all-defined-out))

(require racket/function)

(define (n-queens n)
  (let ((results '()))
    (define (reject state)
      (and (not (null? state))
           (let ((i (car state)))
             (or (collide-right state i)
                 (collide-up state i)
                 (collide-down state i)))))

    (define ((collide next invalid) state i)
      (let loop ((i (next i)) (state (cdr state)))
        (and (not (null? state))
             (not (invalid i))
             (or (= i (car state))
                 (loop (next i) (cdr state))))))

    (define collide-right
      (collide identity (const #f)))

    (define collide-up
      (collide add1 (lambda (i) (>= i n))))

    (define collide-down
      (collide sub1 (lambda (i) (negative? i))))

    (define (accept state)
      (and (= n (length state))
           (not (reject state))))

    (define (children state)
      (if (= n (length state))
          '()
          (for/list ((i (in-range n)))
            (cons i state))))

    (define (return state)
      (set! results (cons state results)))

    (let loop ((state '()))
      (unless (reject state)
        (if (accept state)
            (return state)
            (for-each loop (children state)))))

    results))
