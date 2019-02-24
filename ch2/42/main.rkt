#lang racket/base

; Exercise 2.42

(provide (all-defined-out))

(require racket/function)

(define size 8)

(define (queens)
  (backtrack '() reject accept children return))

(define (backtrack state reject accept children return)
  (let loop ((state state))
    (unless (reject state)
      (if (accept state)
          (return state)
          (for-each loop (children state))))))

(define (reject state)
  (and (not (null? state))
       (let ((i (car state)))
         (or (collide-up state i)
             (collide-right state i)
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
  (collide add1 (lambda (i) (>= i size))))

(define collide-down
  (collide sub1 (lambda (i) (negative? i))))

(define (accept state)
  (and (= size (length state))
       (not (reject state))))

(define (children state)
  (if (= size (length state))
      '()
      (for/list ((i (in-range size)))
        (cons i state))))

(define (return state)
  (for ((i state))
    (printf "~a " i))
  (newline))
