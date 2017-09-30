#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (add-interval i j)
  (make-interval (+ (lower-bound i)
                    (lower-bound j))
                 (+ (upper-bound i)
                    (upper-bound j))))

(define (sub-interval i j)
  (make-interval (- (lower-bound i)
                    (upper-bound j))
                 (- (upper-bound i)
                    (lower-bound j))))

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
  (let ((jl (lower-bound j))
        (jh (upper-bound j)))
    (if (or (zero? jl)
            (zero? jh)
            (and (negative? jl)
                 (positive? jh)))
        (error "div-interval: division by zero")
        (mul-interval i
                      (make-interval (/ 1.0 jh)
                                     (/ 1.0 jl))))))

(div-interval (make-interval 0 0)
              (make-interval -1 1))
;; div-interval: division by zero
