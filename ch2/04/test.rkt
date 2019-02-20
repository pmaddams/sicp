#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "equal?"
 (check equal? 1 1)
 (check equal? #f (equal? 1 2))
 (check equal? (cons 1 (cons 2 '())) (cons 1 (cons 2 '())))
 (check equal? #f (equal? (cons 1 (cons 2 '())) (cons 1 (cons 2 (cons 3 '()))))))

(test-case
 "set-car!"
 (let ((p (cons 1 '())))
   (set-car! p 2)
   (check equal? p (cons 2 '())))
 (let ((p (cons 1 (cons 2 '()))))
   (set-car! (cdr p) 3)
   (check equal? p (cons 1 (cons 3 '())))))

(test-case
 "set-cdr!"
 (let ((p (cons 1 '())))
   (set-cdr! p (cons 2 '()))
   (check equal? p (cons 1 (cons 2 '()))))
 (let ((p (cons 1 (cons 2 '()))))
   (set-cdr! (cdr p) (cons 3 '()))
   (check equal? p (cons 1 (cons 2 (cons 3 '()))))))

(test-case
 "member"
 (check equal? (member 2 (cons 1 (cons 2 (cons 3 '())))) (cons 2 (cons 3 '())))
 (check equal? #f (member 3 (cons 1 (cons 2 '())))))

(test-case
 "remove"
 (check equal? (remove 2 (cons 1 (cons 2 (cons 3 '())))) (cons 1 (cons 3 '())))
 (check equal? (remove 3 (cons 1 (cons 2 '()))) (cons 1 (cons 2 '()))))

(test-case
 "length"
 (check equal? (length '()) 0)
 (check equal? (length (cons 1 (cons 2 (cons 3 '())))) 3))

(test-case
 "append"
 (check equal? (append '() '()) '())
 (check equal? (append (cons 1 '()) (cons 2 '())) (cons 1 (cons 2 '()))))

(test-case
 "reverse"
 (check equal? (reverse '()) '())
 (check equal? (reverse (cons 1 (cons 2 (cons 3 '())))) (cons 3 (cons 2 (cons 1 '())))))

(define (square n) (* n n))

(test-case
 "map"
 (let ((l (cons 1 (cons 2 (cons 3 '())))))
   (check equal? (map add1 l) (cons 2 (cons 3 (cons 4 '()))))
   (check equal? (map square l) (cons 1 (cons 4 (cons 9 '()))))))

(test-case
 "filter"
 (let ((l (cons 1 (cons 2 (cons 3 '())))))
   (check equal? (filter odd? l) (cons 1 (cons 3 '())))
   (check equal? (filter even? l) (cons 2 '()))))

(test-case
 "sum"
 (check equal? (sum (cons 1 (cons 2 (cons 3 '())))) 6)
 (check equal? (sum (cons 4 (cons 5 (cons 6 '())))) 15))

(test-case
 "product"
 (check equal? (product (cons 1 (cons 2 (cons 3 '())))) 6)
 (check equal? (product (cons 4 (cons 5 (cons 6 '())))) 120))
