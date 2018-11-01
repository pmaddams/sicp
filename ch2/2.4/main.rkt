#lang racket/base

(provide cons car cdr set-car! set-cdr! null null? equal?
         length member remove map filter
         append reverse sum product)

(define (cons x y)
  (lambda (f)
    (f x y (lambda (z) (set! x z)) (lambda (z) (set! y z)))))

(define (car p)
  (p (lambda (x y set-x set-y) x)))

(define (cdr p)
  (p (lambda (x y set-x set-y) y)))

(define (set-car! p z)
  (p (lambda (x y set-x set-y) (set-x z))))

(define (set-cdr! p z)
  (p (lambda (x y set-x set-y) (set-y z))))

(define (null) 'null)

(define (null? l)
  (eq? l 'null))

(define (equal? x y)
  (cond ((number? x) (and (number? y) (= x y)))
        ((symbol? x) (and (symbol? y) (eq? x y)))
        (else (and (neither (number? y) (symbol? y))
                   (equal? (car x) (car y))
                   (equal? (cdr x) (cdr y))))))

(define-syntax-rule (neither expr ...)
  (not (or expr ...)))

(define (length l)
  (if (null? l)
      0
      (add1 (length (cdr l)))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? (car l) x) l)
        (else (member x (cdr l)))))

(define (remove x l)
  (cond ((null? l) (null))
        ((equal? (car l) x) (cdr l))
        (else (cons (car l) (remove x (cdr l))))))

(define (map f l)
  (if (null? l)
      (null)
      (cons (f (car l)) (map f (cdr l)))))

(define (filter f l)
  (cond ((null? l) (null))
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

(define (append l1 l2)
  (foldr cons l2 l1))

(define (reverse l)
  (foldl cons (null) l))

(define (sum l)
  (foldl + 0 l))

(define (product l)
  (foldl * 1 l))

(define (foldr f z l)
  (if (null? l)
      z
      (f (car l) (foldr f z (cdr l)))))

(define (foldl f z l)
  (if (null? l)
      z
      (foldl f (f (car l) z) (cdr l))))
