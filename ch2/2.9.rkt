#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (width-interval x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (displayln x)
  (display x)
  (newline))

(let ((x (make-interval 1 2))
      (y (make-interval 3 4)))
  (displayln (+ (width-interval x)
                (width-interval y)))
  (displayln (width-interval (add-interval x y)))
  (displayln (width-interval (sub-interval x y)))
  (displayln (width-interval (mul-interval x y)))
  (displayln (width-interval (div-interval x y))))
;; 1
;; 1
;; 1
;; 5/2
;; 0.20833333333333331
