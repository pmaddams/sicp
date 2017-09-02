#lang sicp

(define (f-rec n)
  (letrec ((f (lambda (n)
                (if (< n 3)
                    n
                    (+ (f (dec n))
                       (* 2 (f (- n 2)))
                       (* 3 (f (- n 3))))))))
    (f n)))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i) (cons i result))))))
    (e high '())))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln
          (map f-rec (enumerate-interval 1 10)))
;; 1
;; 2
;; 4
;; 11
;; 25
;; 59
;; 142
;; 335
;; 796
;; 1892

(define (f-iter n)
  (letrec ((next (lambda (a b c)
                (+ c
                   (* 2 b)
                   (* 3 a))))
           (f (lambda (a b c count)
                (if (zero? count)
                    a
                    (f b c (next a b c) (dec count))))))
    (f 0 1 2 n)))

(for-each displayln
          (map f-iter (enumerate-interval 1 10)))
;; 1
;; 2
;; 4
;; 11
;; 25
;; 59
;; 142
;; 335
;; 796
;; 1892
