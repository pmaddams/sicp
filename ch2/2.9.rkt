#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (width in)
  (/ (- (upper-bound in)
        (lower-bound in))
     2))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (width-add-or-sub x y)
  (+ (width x)
     (width y)))

(let ((x (make-interval 1 2))
      (y (make-interval 3 4)))
  (begin (display (width-add-or-sub x y))
         (newline)
         (display (width (add-interval x y)))
         (newline)
         (display (width (sub-interval x y)))
         (newline)
         (display (width (mul-interval x y)))
         (newline)
         (display (width (div-interval x y)))
         (newline)))
;; 1
;; 1
;; 1
;; 5/2
;; 0.20833333333333331