#lang sicp

(define (product-rec term a next b)
  (letrec ((p (lambda (a)
                (if (> a b)
                    1
                    (* (term a)
                       (p (next a)))))))
    (p a)))

(define (product-iter term a next b)
  (letrec ((p (lambda (a result)
                   (if (> a b)
                       result
                       (p (next a) (* (term a) result))))))
    (p a 1)))

(define (factorial n)
  (let ((product product-iter))
    (product identity 1 inc n)))

(define (pi-product a b)
  (let ((product product-iter)
        (pi-term (lambda (x)
                   (/ (* (dec x) (inc x))
                      (* x x))))
        (pi-next (lambda (x)
                   (+ x 2))))
    (product pi-term a pi-next b)))

(* 4 (pi-product 3.0 1000))
;; 3.1431638424192028