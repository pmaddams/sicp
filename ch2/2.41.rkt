#lang sicp

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (filter pred? l)
  (letrec ((f (lambda (l)
                (cond ((null? l) '())
                      ((pred? (car l)) (cons (car l) (f (cdr l))))
                      (else (f (cdr l)))))))
    (f l)))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i) (cons i result))))))
    (e high '())))

(define (flatmap p l)
  (foldr append '() (map p l)))

(define (triples-sum-to-s s n)
  (let ((triples (flatmap (lambda (i)
                            (flatmap (lambda (j)
                                       (map (lambda (k) (list i j k))
                                            (enumerate-interval 1 n)))
                                     (enumerate-interval 1 n)))
                          (enumerate-interval 1 n)))
        (sum-to-s? (lambda (t)
                     (= s (+ (car t) (cadr t) (caddr t))))))
    (filter sum-to-s? triples)))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln (triples-sum-to-s 6 6))
;; (1 1 4)
;; (1 2 3)
;; (1 3 2)
;; (1 4 1)
;; (2 1 3)
;; (2 2 2)
;; (2 3 1)
;; (3 1 2)
;; (3 2 1)
;; (4 1 1)
