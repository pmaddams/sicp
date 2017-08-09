#lang sicp

(define (accumulate op init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (a (cdr seq)))))))
    (a seq)))

(define (filter pred? seq)
  (letrec ((f (lambda (seq)
                (cond ((null? seq) '())
                      ((pred? (car seq)) (cons (car seq) (f (cdr seq))))
                      (else (f (cdr seq)))))))
    (f seq)))

(define (enumerate-interval i lim)
  (letrec ((e (lambda (i)
                (if (> i lim)
                    '()
                    (cons i (e (inc i)))))))
    (e i)))

(define (flatmap p seq)
  (accumulate append '() (map p seq)))

(define (queens board-size)
  (letrec ((empty-board '())
           (adjoin-position (lambda (new-row k rest-of-queens)
                              ()))
           (safe? (lambda (k positions)
                    ()))
           (queen-cols (lambda (k)  
                         (if (zero? k)
                             (list empty-board)
                             (filter
                              (lambda (positions) (safe? k positions))
                              (flatmap
                               (lambda (rest-of-queens)
                                 (map (lambda (new-row)
                                        (adjoin-position new-row k rest-of-queens))
                                      (enumerate-interval 1 board-size)))
                               (queen-cols (dec k))))))))
    (queen-cols board-size)))