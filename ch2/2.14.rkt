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
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y))
        (nonneg? (lambda (n)
                   (or (zero? n)
                       (positive? n)))))
    (cond ((nonneg? x1)
           (cond ((nonneg? y1) (make-interval (* x1 y1)
                                              (* x2 y2)))
                 ((negative? y2) (make-interval (* x2 y1)
                                                (* x1 y2)))
                 (else (make-interval (* x2 y1)
                                      (* x2 y2)))))
          ((negative? x2)
           (cond ((nonneg? y1) (make-interval (* x1 y2)
                                              (* x2 y1)))
                 ((negative? y2) (make-interval (* x2 y2)
                                                (* x1 y1)))
                 (else (make-interval (* x1 y2)
                                      (* x1 y1)))))
          (else
           (cond ((nonneg? y1) (make-interval (* x1 y2)
                                              (* x2 y2)))
                 ((negative? y2) (make-interval (* x2 y1)
                                                (* x1 y1)))
                 (else (make-interval (let ((a (* x1 y2))
                                            (b (* x2 y1)))
                                        (if (< a b)
                                            a
                                            b))
                                      (let ((a (* x1 y1))
                                            (b (* x2 y2)))
                                        (if (> a b)
                                            a
                                            b)))))))))

(define (div-interval x y)
  (if (or (zero? (lower-bound y))
          (zero? (upper-bound x)))
      (error "division by zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let* ((fraction (/ p 100))
         (lower (- c (* c fraction)))
         (upper (+ c (* c fraction))))
    (make-interval lower upper)))

(define (percent i)
  (* 100 (/ (width i) (center i))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (print-center-percent i)
  (display (center i))
  (display " +- ")
  (display (inexact->exact (round (percent i))))
  (display "%")
  (newline))

(let ((r1 (make-center-percent 1000 30))
      (r2 (make-center-percent 2000 30)))
  (print-center-percent (par1 r1 r2))
  (print-center-percent (par2 r1 r2)))
;; 930.4029304029305 +- 73%
;; 666.6666666666667 +- 30%

(let ((a (make-center-percent 3000 1))
      (b (make-center-percent 3000 20)))
  (print-center-percent (div-interval a a))
  (print-center-percent (div-interval a b))
  (print-center-percent (div-interval b a))
  (print-center-percent (div-interval b b)))
;; 1.0002000200020003 +- 2%
;; 1.0437500000000002 +- 21%
;; 1.0021002100210021 +- 21%
;; 1.0833333333333333 +- 38%