#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (add-interval i j)
  (make-interval (+ (lower-bound i) (lower-bound j))
                 (+ (upper-bound i) (upper-bound j))))

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
  (print-interval (add-interval i j))
  (print-interval (mul-interval i j))
  (print-interval (div-interval i j)))
;; [4, 6]
;; [3, 8]
;; [0.25, 0.6666666666666666]
