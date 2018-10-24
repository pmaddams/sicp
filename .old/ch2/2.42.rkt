#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (flatmap proc l)
  (foldr append '() (map proc l)))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i)
                       (cons i result))))))
    (e high '())))

(define (filter pred? l)
  (letrec ((f (lambda (l result)
                (cond ((null? l)
                       result)
                      ((pred? (car l))
                       (f (cdr l)
                          (cons (car l) result)))
                      (else
                       (f (cdr l)
                          result))))))
    (f l '())))

(define (queens board-size)
  (let ((empty-board '())
        (adjoin-position cons)
        (safe? (lambda (positions)
                 (let ((new-row (car positions))
                       (rest-of-queens (cdr positions)))
                   (letrec ((safe-right? (lambda (rest-of-queens)
                                           (or (null? rest-of-queens)
                                               (and (not (= new-row (car rest-of-queens)))
                                                    (safe-right? (cdr rest-of-queens))))))
                            (safe-up? (lambda (check-row rest-of-queens)
                                        (or (null? rest-of-queens)
                                            (zero? check-row)
                                            (and (not (= check-row (car rest-of-queens)))
                                                 (safe-up? (dec check-row) (cdr rest-of-queens))))))
                            (safe-down? (lambda (check-row rest-of-queens)
                                          (or (null? rest-of-queens)
                                              (> check-row board-size)
                                              (and (not (= check-row (car rest-of-queens)))
                                                   (safe-down? (inc check-row) (cdr rest-of-queens)))))))
                     (and (safe-right? rest-of-queens)
                          (safe-up? (dec new-row) rest-of-queens)
                          (safe-down? (inc new-row) rest-of-queens)))))))
    (letrec ((queen-cols (lambda (k)  
                           (if (zero? k)
                               (list empty-board)
                               (filter safe?
                                       (flatmap
                                        (lambda (rest-of-queens)
                                          (map (lambda (new-row)
                                                 (adjoin-position new-row rest-of-queens))
                                               (enumerate-interval 1 board-size)))
                                        (queen-cols (dec k))))))))
      (queen-cols board-size))))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln (queens 8))
;; (4 2 7 3 6 8 5 1)
;; (5 2 4 7 3 8 6 1)
;; (3 5 2 8 6 4 7 1)
;; (3 6 4 2 8 5 7 1)
;; (5 7 1 3 8 6 4 2)
;; (4 6 8 3 1 7 5 2)
;; (3 6 8 1 4 7 5 2)
;; (5 3 8 4 7 1 6 2)
;; (5 7 4 1 3 8 6 2)
;; (4 1 5 8 6 3 7 2)
;; (3 6 4 1 8 5 7 2)
;; (4 7 5 3 1 6 8 2)
;; (6 4 2 8 5 7 1 3)
;; (6 4 7 1 8 2 5 3)
;; (1 7 4 6 8 2 5 3)
;; (6 8 2 4 1 7 5 3)
;; (6 2 7 1 4 8 5 3)
;; (4 7 1 8 5 2 6 3)
;; (5 8 4 1 7 2 6 3)
;; (4 8 1 5 7 2 6 3)
;; (2 7 5 8 1 4 6 3)
;; (1 7 5 8 2 4 6 3)
;; (2 5 7 4 1 8 6 3)
;; (4 2 7 5 1 8 6 3)
;; (5 7 1 4 2 8 6 3)
;; (6 4 1 5 8 2 7 3)
;; (5 1 4 6 8 2 7 3)
;; (5 2 6 1 7 4 8 3)
;; (6 3 7 2 8 5 1 4)
;; (2 7 3 6 8 5 1 4)
;; (7 3 1 6 8 5 2 4)
;; (5 1 8 6 3 7 2 4)
;; (1 5 8 6 3 7 2 4)
;; (3 6 8 1 5 7 2 4)
;; (6 3 1 7 5 8 2 4)
;; (7 5 3 1 6 8 2 4)
;; (7 3 8 2 5 1 6 4)
;; (5 3 1 7 2 8 6 4)
;; (2 5 7 1 3 8 6 4)
;; (3 6 2 5 8 1 7 4)
;; (6 1 5 2 8 3 7 4)
;; (8 3 1 6 2 5 7 4)
;; (2 8 6 1 3 5 7 4)
;; (5 7 2 6 3 1 8 4)
;; (3 6 2 7 5 1 8 4)
;; (6 2 7 1 3 5 8 4)
;; (3 7 2 8 6 4 1 5)
;; (6 3 7 2 4 8 1 5)
;; (4 2 7 3 6 8 1 5)
;; (7 1 3 8 6 4 2 5)
;; (1 6 8 3 7 4 2 5)
;; (3 8 4 7 1 6 2 5)
;; (6 3 7 4 1 8 2 5)
;; (7 4 2 8 6 1 3 5)
;; (4 6 8 2 7 1 3 5)
;; (2 6 1 7 4 8 3 5)
;; (2 4 6 8 3 1 7 5)
;; (3 6 8 2 4 1 7 5)
;; (6 3 1 8 4 2 7 5)
;; (8 4 1 3 6 2 7 5)
;; (4 8 1 3 6 2 7 5)
;; (2 6 8 3 1 4 7 5)
;; (7 2 6 3 1 4 8 5)
;; (3 6 2 7 1 4 8 5)
;; (4 7 3 8 2 5 1 6)
;; (4 8 5 3 1 7 2 6)
;; (3 5 8 4 1 7 2 6)
;; (4 2 8 5 7 1 3 6)
;; (5 7 2 4 8 1 3 6)
;; (7 4 2 5 8 1 3 6)
;; (8 2 4 1 7 5 3 6)
;; (7 2 4 1 8 5 3 6)
;; (5 1 8 4 2 7 3 6)
;; (4 1 5 8 2 7 3 6)
;; (5 2 8 1 4 7 3 6)
;; (3 7 2 8 5 1 4 6)
;; (3 1 7 5 8 2 4 6)
;; (8 2 5 3 1 7 4 6)
;; (3 5 2 8 1 7 4 6)
;; (3 5 7 1 4 2 8 6)
;; (5 2 4 6 8 3 1 7)
;; (6 3 5 8 1 4 2 7)
;; (5 8 4 1 3 6 2 7)
;; (4 2 5 8 6 1 3 7)
;; (4 6 1 5 2 8 3 7)
;; (6 3 1 8 5 2 4 7)
;; (5 3 1 6 8 2 4 7)
;; (4 2 8 6 1 3 5 7)
;; (6 3 5 7 1 4 2 8)
;; (6 4 7 1 3 5 2 8)
;; (4 7 5 2 6 1 3 8)
;; (5 7 2 6 3 1 4 8)