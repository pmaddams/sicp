#lang sicp

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
  (let ((yl (lower-bound y))
        (yh (upper-bound y)))
    (if (or (zero? yl)
            (zero? yh)
            (and (negative? yl)
                 (positive? yh)))
        (error "div-interval: division by zero")
        (mul-interval x
                      (make-interval (/ 1.0 yh)
                                     (/ 1.0 yl))))))

(div-interval (make-interval 0 0)
              (make-interval -1 1))
;; div-interval: division by zero
