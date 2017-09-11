#lang sicp

;; Let z = x - y, where x, y, and z are intervals. The lower bound of z is equal
;; to the lower bound of x minus the upper bound of y, and the upper bound of z
;; is equal to the upper bound of x minus the lower bound of y.

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xh (upper-bound x))
        (yl (lower-bound y))
        (yh (upper-bound y)))
    (let ((p1 (* xl yl))
          (p2 (* xl yh))
          (p3 (* xh yl))
          (p4 (* xh yh)))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4)))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]")
  (newline))

(let ((x (make-interval 1 2))
      (y (make-interval 3 4)))
  (print-interval (sub-interval x y)))
;; [-3, -1]
