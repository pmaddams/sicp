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

(let ((make-and-mul (lambda (a b c d)
                      (begin (display (mul-interval (make-interval a b)
                                                    (make-interval c d)))
                             (newline)))))
  (make-and-mul 1 2 3 4)
  (make-and-mul 1 2 -4 -3)
  (make-and-mul 1 2 -3 4)
  (make-and-mul -2 -1 3 4)
  (make-and-mul -2 -1 -4 -3)
  (make-and-mul -2 -1 -3 4)
  (make-and-mul -1 2 3 4)
  (make-and-mul -1 2 -4 -3)
  (make-and-mul -1 2 -3 4))
;; (3 . 8)
;; (-8 . -3)
;; (-6 . 8)
;; (-8 . -3)
;; (3 . 8)
;; (-8 . 6)
;; (-4 . 8)
;; (-8 . 4)
;; (-6 . 8)