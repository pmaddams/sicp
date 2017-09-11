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

(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ", ")
  (display (upper-bound x))
  (display "]")
  (newline))

(for-each print-interval
          (map (lambda (x)
                 (apply
                  (lambda (a b c d)
                    (mul-interval (make-interval a b)
                                  (make-interval c d)))
                  x))
               '((1 2 3 4)
                 (1 2 -4 -3)
                 (1 2 -3 4)
                 (-2 -1 3 4)
                 (-2 -1 -4 -3)
                 (-2 -1 -3 4)
                 (-1 2 3 4)
                 (-1 2 -4 -3)
                 (-1 2 -3 4))))
;; [3, 8]
;; [-8, -3]
;; [-6, 8]
;; [-8, -3]
;; [3, 8]
;; [-8, 6]
;; [-4, 8]
;; [-8, 4]
;; [-6, 8]
