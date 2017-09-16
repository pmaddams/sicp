#lang sicp

(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (width-interval i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

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

(define (displayln i)
  (display i)
  (newline))

(let ((i (make-interval 1 2))
      (j (make-interval 3 4)))
  (for-each displayln
            (list (+ (width-interval i)
                     (width-interval j))
                  (width-interval (add-interval i j))
                  (width-interval (sub-interval i j))
                  (width-interval (mul-interval i j))
                  (width-interval (div-interval i j)))))
;; 1
;; 1
;; 1
;; 5/2
;; 0.20833333333333331
