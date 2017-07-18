#lang sicp

(define (pascal row col)
  (letrec ((p (lambda (row col)
                (if (or (= row 1)
                        (= col 1)
                        (= row col))
                    1
                    (+ (p (dec row) (dec col))
                       (p (dec row) col))))))
    (if (and (> col 0)
             (>= row col))
        (p row col)
        0)))