#lang racket/base

(define us-coins '(1 5 10 25 50))

(define uk-coins '(0.5 1 2 5 10 20 50 100))

(define (count amount coins)
  (let loop ((amount amount) (coins (dedup coins)))
    (cond ((or (negative? amount) (null? coins)) 0)
          ((zero? amount) 1)
          (else (+ (loop amount (cdr coins))
                   (loop (- amount (car coins)) coins))))))

(define (dedup l)
  (let loop ((l l) (acc '()))
    (if (null? l)
        acc
        (loop (remove* (list (car l)) (cdr l))
              (cons (car l) acc)))))
