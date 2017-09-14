#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (foldr-n proc init ls)
  (let ((f (lambda (l)
             (foldr proc init l))))
    (letrec ((fn (lambda (ls)
                   (if (null? (car ls))
                       '()
                       (cons (f (map car ls))
                             (fn (map cdr ls)))))))
      (fn ls))))

(define (dot-product v1 v2)
  (foldr + 0 (map * v1 v2)))

(define (matrix-*-vector m v)
  (map (lambda (r)
         (dot-product r v))
       m))

(define (transpose m)
  (foldr-n cons '() m))

(define (matrix-*-matrix m1 m2)
  (let ((rows m1)
        (cols (transpose m2)))
    (map (lambda (r)
           (matrix-*-vector cols r))
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
