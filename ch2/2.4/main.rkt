#lang racket

(define (map f l)
  (if (null? l)
      (null)
      (cons (f (car l)) (map f (cdr l)))))

(define (filter f l)
  (cond ((null? l) (null))
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (foldr f z l)
  (if (null? l)
      z
      (f (car l) (foldr f z (cdr l)))))

(define (foldl f z l)
  (if (null? l)
      z
      (foldl f (f (car l) z) (cdr l))))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (reverse l)
  (foldl cons (null) l))

(define (sum l)
  (foldl + 0 l))

(define (product l)
  (foldl * 1 l))

(define (length l)
  (sum (map (const 1) l)))

(define (equal? x y)
  (cond ((number? x) (and (number? y) (= x y)))
        ((symbol? x) (and (symbol? y) (eq? x y)))
        (else (and (neither (number? y) (symbol? y))
                   (equal? (car x) (car y))
                   (equal? (cdr x) (cdr y))))))

(define-syntax-rule (neither expr ...)
  (not (or expr ...)))

(define (cons x y)
  (lambda (f)
    (f x y)))

(define (car p)
  (p (lambda (x y) x)))

(define (cdr p)
  (p (lambda (x y) y)))

(define (null) 'null)

(define (null? l)
  (eq? l 'null))
