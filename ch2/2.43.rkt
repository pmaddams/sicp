#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

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

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i) (cons i result))))))
    (e high '())))

(define (flatmap p l)
  (foldr append '() (map p l)))

(define (queens-compare-steps board-size)
  (letrec ((counter 0)
           (empty-board '())
           (adjoin-position cons)
           (safe? (lambda (positions)
                    (letrec ((new-row (car positions))
                             (rest-of-queens (cdr positions))
                             (safe-right? (lambda (rest-of-queens)
                                            (if (null? rest-of-queens)
                                                #t
                                                (and (not (= new-row (car rest-of-queens)))
                                                     (safe-right? (cdr rest-of-queens))))))
                             (safe-up? (lambda (check-row rest-of-queens)
                                         (if (or (null? rest-of-queens)
                                                 (zero? check-row))
                                             #t
                                             (and (not (= check-row (car rest-of-queens)))
                                                  (safe-up? (dec check-row) (cdr rest-of-queens))))))
                             (safe-down? (lambda (check-row rest-of-queens)
                                           (if (or (null? rest-of-queens)
                                                   (> check-row board-size))
                                               #t
                                               (and (not (= check-row (car rest-of-queens)))
                                                    (safe-down? (inc check-row) (cdr rest-of-queens)))))))
                      (and (safe-right? rest-of-queens)
                           (safe-up? (dec new-row) rest-of-queens)
                           (safe-down? (inc new-row) rest-of-queens)))))
           (good-cols (lambda (k)
                        (set! counter (inc counter))
                        (if (zero? k)
                            (list empty-board)
                            (filter safe?
                                    (flatmap
                                     (lambda (rest-of-queens)
                                       (map (lambda (new-row)
                                              (adjoin-position new-row rest-of-queens))
                                            (enumerate-interval 1 board-size)))
                                     (good-cols (dec k)))))))
           (bad-cols (lambda (k)
                       (set! counter (inc counter))
                       (if (zero? k)
                           (list empty-board)
                           (filter safe?
                                   (flatmap
                                    (lambda (new-row)
                                      (map (lambda (rest-of-queens)
                                             (adjoin-position new-row rest-of-queens))
                                           (bad-cols (dec k))))
                                    (enumerate-interval 1 board-size)))))))
    (good-cols board-size)
    (display counter)
    (display " ")
    (set! counter 0)
    (bad-cols board-size)
    (display counter)
    (newline)))

(for-each queens-compare-steps (enumerate-interval 1 8))
;; 2 2
;; 3 7
;; 4 40
;; 5 341
;; 6 3906
;; 7 55987
;; 8 960800
;; 9 19173961

;; The flatmap operates on the enumerated interval from 1 to board-size. The
;; procedure mapped to this interval takes the argument new-row, an integer, and
;; returns the mapping of an inner procedure to the result of queen-cols for the
;; previous value of k. This inner procedure adjoins the new row to each member
;; of the previously calculated result. As a result, instead of evaluating
;; queen-cols once for each value of k down to 0, queen-cols is called an
;; exponential number of times. Therefore, if it takes the original program T
;; time to run, a reasonable estimate of the new running time is T^T.
