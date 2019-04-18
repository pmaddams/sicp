#lang lisp

(define (make-check name)
  (lambda (x y)
    (if (equal? x y)
        'ok
        (begin (displayln 'FAILURE)
               (displayln name)))))

(define (equal? x y)
  (if (pair? x)
      (and (pair? y)
           (equal? (car x) (car y))
           (equal? (cdr x) (cdr y)))
      (eq? x y)))

(define (displayln x)
  (display x)
  (newline))

(let ((check (make-check 'literals)))
  (check #t #t)
  (check #f #f)
  (check 0.0 0.0))

(let ((check (make-check 'quote)))
  (check 'foo 'foo)
  (check '(1 (2 3) 4 5) '(1 (2 3) 4 5)))

(let ((check (make-check 'lambda)))
  (check ((lambda (n)
            (* n n))
          2)
         4)
  (check ((lambda (n)
            ((lambda (m)
               (* n m))
             2))
          3)
         6))

(let ((check (make-check 'define)))
  (begin (define (square n) (* n n))
         (check (square 2) 4))
  (begin (define n 2)
         (define m 3)
         (check (* n m) 6)))

(let ((check (make-check 'set!)))
  (begin (define n 2)
         (set! n (+ n 1))
         (check n 3))
  (check (let ((n 2))
           (set! n (+ n 1))
           n)
         3))

(let ((check (make-check 'if)))
  (check (if (< 1 2)
             'foo
             (/ 1 0))
         'foo)
  (begin (define (map f l)
           (if (null? l)
               '()
               (cons (f (car l)) (map f (cdr l)))))
         (check (map (lambda (n) (* n 2))
                     '(1 2 3 4 5))
                '(2 4 6 8 10))))

(let ((check (make-check 'begin)))
  (check (begin #t #f)
         #f)
  (check (if (< 1 2)
             (begin 'foo
                    'bar)
             (/ 1 0))
         'bar))

(let ((check (make-check 'cond)))
  (begin (define (memq x l)
           (cond ((null? l) #f)
                 ((eq? x (car l)) l)
                 (else (memq x (cdr l)))))
         (check (let ((x (memq 3 '(1 2 3 4 5))))
                  (and x (car x)))
                3)
         (check (let ((x (memq 6 '(1 2 3 4 5))))
                  (and x (car x)))
                #f)))

(let ((check (make-check 'and)))
  (check (and)
         #t)
  (check (if (and (< 1 2)
                  (eq? 'a 'a)
                  (not (null? '(3 4 5))))
             'foo
             'bar)
         'foo)
  (check (if (and (< 1 2)
                  (eq? 'a 'b)
                  (/ 1 0))
             'foo
             'bar)
         'bar))

(let ((check (make-check 'or)))
  (check (or)
         #f)
  (check (if (or (< 1 2)
                 (/ 1 0)
                 (null? (cdr '())))
             'foo
             'bar)
         'foo)
  (check (if (or (< 2 1)
                 (eq? 'a 'b)
                 (null? '(3 4 5)))
             'foo
             'bar)
         'bar))

(let ((check (make-check 'let)))
  (check (let ((n 1))
           (let ((n 2)
                 (m (+ n 1)))
             m))
         2)
  (check (let ((square (lambda (n) (* n n))))
           (square 2)
           (square 3))
         9))