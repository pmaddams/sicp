#lang sicp

(define (accumulate op init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (a (cdr seq)))))))
    (a seq)))

(define (accumulate-n op init seqs)
  (letrec ((a (lambda (seq)
                (accumulate op init seq)))
           (an (lambda (seqs)
                 (if (null? (car seqs))
                     '()
                     (cons (a (map car seqs))
                           (an (map cdr seqs)))))))
    (an seqs)))

(define (dot-product v1 v2)
  (accumulate + 0 (map * v1 v2)))

(define (matrix-*-vector m v)
  (let ((p (lambda (r)
             (dot-product r v))))
    (map p m)))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m1 m2)
  (let* ((rows m1)
        (cols (transpose m2)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         rows)))

(define (displayln x)
  (display x)
  (newline))

(let ((m '((1 2 3 4) (4 5 6 6) (6 7 8 9))))
  (displayln (matrix-*-vector m (car m)))
  (displayln (transpose m))
  (displayln (matrix-*-matrix m (transpose m))))
;; (30 56 80)
;; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
;; ((30 56 80) (56 113 161) (80 161 230))