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
        (yh (upper-bound y))
        (nonneg? (lambda (n)
                   (or (zero? n)
                       (positive? n)))))
    (cond ((nonneg? xl)
           (cond ((nonneg? yl) (make-interval (* xl yl)
                                              (* xh yh)))
                 ((negative? yh) (make-interval (* xh yl)
                                                (* xl yh)))
                 (else (make-interval (* xh yl)
                                      (* xh yh)))))
          ((negative? xh)
           (cond ((nonneg? yl) (make-interval (* xl yh)
                                              (* xh yl)))
                 ((negative? yh) (make-interval (* xh yh)
                                                (* xl yl)))
                 (else (make-interval (* xl yh)
                                      (* xl yl)))))
          (else
           (cond ((nonneg? yl) (make-interval (* xl yh)
                                              (* xh yh)))
                 ((negative? yh) (make-interval (* xh yl)
                                                (* xl yl)))
                 (else (make-interval (let ((a (* xl yh))
                                            (b (* xh yl)))
                                        (if (< a b)
                                            a
                                            b))
                                      (let ((a (* xl yl))
                                            (b (* xh yh)))
                                        (if (> a b)
                                            a
                                            b)))))))))

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

(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2.0))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2.0))

(define (make-center-percent c p)
  (let* ((fraction (/ p 100))
         (low (- c (* c fraction)))
         (high (+ c (* c fraction))))
    (make-interval low high)))

(define (percent i)
  (* 100.0 (/ (width i)
            (center i))))

(define (print-center-percent i)
  (display (center i))
  (display " +/- ")
  (display (percent i))
  (display "%")
  (newline))

(let ((i (make-interval 2 3)))
  (print-center-percent (make-center-percent (center i)
                                             (percent i))))
;; 2.5 +/- 20.0%
