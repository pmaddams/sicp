#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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
  (print-interval (add-interval x y))
  (print-interval (mul-interval x y))
  (print-interval (div-interval x y)))
;; [4, 6]
;; [3, 8]
;; [0.25, 0.6666666666666666]
