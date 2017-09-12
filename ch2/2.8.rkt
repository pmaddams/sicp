#lang sicp

;; Let k = i - j, where i, j, and k are intervals. The lower bound of k is equal
;; to the lower bound of i minus the upper bound of j, and the upper bound of k
;; is equal to the upper bound of i minus the lower bound of j.

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (add-interval i j)
  (make-interval (+ (lower-bound i) (lower-bound j))
                 (+ (upper-bound i) (upper-bound j))))

(define (sub-interval i j)
  (make-interval (- (lower-bound i) (upper-bound j))
                 (- (upper-bound i) (lower-bound j))))

(define (mul-interval i j)
  (let ((il (lower-bound i))
        (ih (upper-bound i))
        (jl (lower-bound j))
        (jh (upper-bound j)))
    (let ((p1 (* il jl))
          (p2 (* il jh))
          (p3 (* ih jl))
          (p4 (* ih jh)))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4)))))

(define (div-interval i j)
  (mul-interval i
                (make-interval (/ 1.0 (upper-bound j))
                               (/ 1.0 (lower-bound j)))))

(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]")
  (newline))

(let ((i (make-interval 1 2))
      (j (make-interval 3 4)))
  (print-interval (sub-interval i j)))
;; [-3, -1]
