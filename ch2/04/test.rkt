#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "equal?"
 (check equal? 1 1)
 (check-false (equal? 1 2))
 (check equal? (cons 1 (cons 2 (null))) (cons 1 (cons 2 (null))))
 (check-false (equal? (cons 1 (cons 2 (null))) (cons 1 (cons 2 (cons 3 (null)))))))

(test-case
 "set-car!"
 (let ((p (cons 1 (null))))
   (set-car! p 2)
   (check equal? p (cons 2 (null))))
 (let ((p (cons 1 (cons 2 (null)))))
   (set-car! (cdr p) 3)
   (check equal? p (cons 1 (cons 3 (null))))))

(test-case
 "set-cdr!"
 (let ((p (cons 1 (null))))
   (set-cdr! p (cons 2 (null)))
   (check equal? p (cons 1 (cons 2 (null)))))
 (let ((p (cons 1 (cons 2 (null)))))
   (set-cdr! (cdr p) (cons 3 (null)))
   (check equal? p (cons 1 (cons 2 (cons 3 (null)))))))

(test-case
 "length"
 (check = (length (null)) 0)
 (check = (length (cons 1 (cons 2 (cons 3 (null))))) 3))

(test-case
 "member"
 (check equal? (member 2 (cons 1 (cons 2 (cons 3 (null))))) (cons 2 (cons 3 (null))))
 (check-false (member 3 (cons 1 (cons 2 (null))))))

(test-case
 "remove"
 (check equal? (remove 2 (cons 1 (cons 2 (cons 3 (null))))) (cons 1 (cons 3 (null))))
 (check equal? (remove 3 (cons 1 (cons 2 (null)))) (cons 1 (cons 2 (null)))))

(define (square n) (* n n))

(test-case
 "map"
 (let ((l (cons 1 (cons 2 (cons 3 (null))))))
   (check equal? (map add1 l) (cons 2 (cons 3 (cons 4 (null)))))
   (check equal? (map square l) (cons 1 (cons 4 (cons 9 (null)))))))

(test-case
 "filter"
 (let ((l (cons 1 (cons 2 (cons 3 (null))))))
   (check equal? (filter odd? l) (cons 1 (cons 3 (null))))
   (check equal? (filter even? l) (cons 2 (null)))))

(test-case
 "append"
 (check equal? (append (null) (null)) (null))
 (check equal? (append (cons 1 (null)) (cons 2 (null))) (cons 1 (cons 2 (null)))))

(test-case
 "reverse"
 (check equal? (reverse (null)) (null))
 (check equal? (reverse (cons 1 (cons 2 (cons 3 (null))))) (cons 3 (cons 2 (cons 1 (null))))))

(test-case
 "sum"
 (check = (sum (cons 1 (cons 2 (cons 3 (null))))) 6)
 (check = (sum (cons 4 (cons 5 (cons 6 (null))))) 15))

(test-case
 "product"
 (check = (product (cons 1 (cons 2 (cons 3 (null))))) 6)
 (check = (product (cons 4 (cons 5 (cons 6 (null))))) 120))
