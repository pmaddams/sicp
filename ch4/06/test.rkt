#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "literals"
 (check-equal? (interpret '(#t)) #t)
 (check-equal? (interpret '(#f)) #f)
 (let ((n (random)))
   (check-equal? (interpret `(,n)) n)))

(test-case
 "quote"
 (check-equal? (interpret '('foo)) 'foo)
 (check-equal? (interpret '('(1 (2 3) 4 5))) '(1 (2 3) 4 5)))

(test-case
 "lambda"
 (check-equal? (interpret '(((lambda (n)
                               (* n n))
                             2)))
               4)
 (check-equal? (interpret '((((lambda (n)
                                (lambda (m)
                                  (* n m)))
                              2)
                             3)))
               6))

(test-case
 "define"
 (check-equal? (interpret '((define (square n) (* n n))
                            (square 2)))
               4)
 (check-equal? (interpret '((define n 2)
                            (define m 3)
                            (* n m)))
               6))

(test-case
 "set!"
 (check-equal? (interpret '((define n 2)
                            (set! n (+ n 1))
                            n))
               3)
 (check-equal? (interpret '((let ((n 2))
                              (set! n (+ n 1))
                              n)))
               3))

(test-case
 "if"
 (check-equal? (interpret '((if (< 1 2)
                                'foo
                                (/ 1 0))))
               'foo)
 (check-equal? (interpret '((define (map f l)
                              (if (null? l)
                                  '()
                                  (cons (f (car l)) (map f (cdr l)))))
                            (map (lambda (n) (* n 2))
                                 '(1 2 3 4 5))))
               '(2 4 6 8 10)))

(test-case
 "begin"
 (check-equal? (interpret '((begin #t #f)))
               #f)
 (check-equal? (interpret '((if (< 1 2)
                                (begin 'foo
                                       'bar)
                                (/ 1 0))))
               'bar))

(test-case
 "cond"
 (check-equal? (interpret '((define (memq x l)
                              (cond ((null? l) #f)
                                    ((eq? x (car l)) l)
                                    (else (memq x (cdr l)))))
                            (let ((x (memq 3 '(1 2 3 4 5))))
                              (and x (car x)))))
               3)
 (check-equal? (interpret '((define (memq x l)
                              (cond ((null? l) #f)
                                    ((eq? x (car l)) l)
                                    (else (memq x (cdr l)))))
                            (let ((x (memq 6 '(1 2 3 4 5))))
                              (and x (car x)))))
               #f))

(test-case
 "and"
 (check-equal? (interpret '((and)))
               #t)
 (check-equal? (interpret '((if (and (< 1 2)
                                     (eq? 'a 'a)
                                     (not (null? '(3 4 5))))
                                'foo
                                'bar)))
               'foo)
 (check-equal? (interpret '((if (and (< 1 2)
                                     (eq? 'a 'b)
                                     (/ 1 0))
                                'foo
                                'bar)))
               'bar))

(test-case
 "or"
 (check-equal? (interpret '((or)))
               #f)
 (check-equal? (interpret '((if (or (< 1 2)
                                    (/ 1 0)
                                    (null? (cdr '())))
                                'foo
                                'bar)))
               'foo)
 (check-equal? (interpret '((if (or (< 2 1)
                                    (eq? 'a 'b)
                                    (null? '(3 4 5)))
                                'foo
                                'bar)))
               'bar))

(test-case
 "let"
 (check-equal? (interpret '((let ((n 1))
                              (let ((n 2)
                                    (m (+ n 1)))
                                m))))
               2)
 (check-equal? (interpret '((let ((square (lambda (n) (* n n))))
                              (square 2)
                              (square 3))))
               9))
