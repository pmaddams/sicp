#lang sicp

(define (square x)
  (expt x 2))

(define (expmod-with-composite-test a n m)
  (letrec ((e (lambda (n)
                (cond ((zero? n) 1)
                      ((even? n) (let* ((x (e (/ n 2)))
                                        (y (remainder (square x) m)))
                                   (if (and (= y 1)
                                            (not (= x 1))
                                            (not (= x (dec m))))
                                       0
                                       y)))
                      (else (remainder (* a (e (dec n))) m))))))
    (e n)))

(define (miller-rabin n)
  (let ((a (inc (random (dec n)))))
    (= 1 (expmod-with-composite-test a (dec n) n))))

(define (prime? n)
  (letrec ((p (lambda (times)
                (or (zero? times)
                    (and (miller-rabin n)
                         (p (dec times)))))))
    (and (> n 1)
         (not (= n 2))
         (odd? n)
         (p 10))))

(define (foldr proc init l)
  (letrec ((f (lambda (l)
                (if (null? l)
                    init
                    (proc (car l)
                          (f (cdr l)))))))
    (f l)))

(define (flatmap proc l)
  (foldr append '() (map proc l)))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i)
                       (cons i result))))))
    (e high '())))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(define first car)

(define second cadr)

(define (make-pair-sum p)
  (list (first p)
        (second p)
        (+ (first p)
           (second p))))

(define (prime-sum? p)
  (prime? (+ (first p)
             (second p))))

(define (filter pred? l)
  (letrec ((f (lambda (l result)
                (cond ((null? l)
                       result)
                      ((pred? (car l))
                       (f (cdr l)
                          (cons (car l) result)))
                      (else
                       (f (cdr l)
                          result))))))
    (f l '())))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln (prime-sum-pairs 6))
;; (6 5 11)
;; (6 1 7)
;; (5 2 7)
;; (4 3 7)
;; (4 1 5)
;; (3 2 5)
;; (2 1 3)
