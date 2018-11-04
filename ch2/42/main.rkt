#lang racket/base

; Exercise 2.42: Eight queens

(require racket/function)

(define size 8)

(define (queens)
  (backtrack '() reject accept children return))

(define (backtrack state reject accept children return)
  (let loop ((state state))
    (and (not (reject state))
         (if (accept state)
             (return state)
             (for-each loop (children state))))))

(define (reject board)
  (and (not (null? board))
       (let ((i (car board)))
         (or (collide-up i board)
             (collide-right i board)
             (collide-down i board)))))

(define (collide invalid next)
  (lambda (i board)
    (let loop ((i (next i)) (board (cdr board)))
      (and (not (null? board))
           (not (invalid i))
           (or (= i (car board))
               (loop (next i) (cdr board)))))))

(define collide-right
  (collide (const #f) identity))

(define collide-up
  (collide (lambda (i) (>= i size)) add1))

(define collide-down
  (collide (lambda (i) (negative? i)) sub1))

(define (accept board)
  (and (= (length board) size)
       (not (reject board))))

(define (children board)
  (if (= (length board) size)
      '()
      (for/list ((i (in-range size)))
        (cons i board))))

(define (return board)
  (for ((i board))
    (printf "~a " (add1 i)))
  (newline))
