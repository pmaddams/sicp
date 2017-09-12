#lang sicp

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
        (jh (upper-bound j))
        (nonneg? (lambda (n)
                   (or (zero? n)
                       (positive? n)))))
    (cond ((nonneg? il)
           (cond ((nonneg? jl) (make-interval (* il jl)
                                              (* ih jh)))
                 ((negative? jh) (make-interval (* ih jl)
                                                (* il jh)))
                 (else (make-interval (* ih jl)
                                      (* ih jh)))))
          ((negative? ih)
           (cond ((nonneg? jl) (make-interval (* il jh)
                                              (* ih jl)))
                 ((negative? jh) (make-interval (* ih jh)
                                                (* il jl)))
                 (else (make-interval (* il jh)
                                      (* il jl)))))
          (else
           (cond ((nonneg? jl) (make-interval (* il jh)
                                              (* ih jh)))
                 ((negative? jh) (make-interval (* ih jl)
                                                (* il jl)))
                 (else (make-interval (let ((a (* il jh))
                                            (b (* ih jl)))
                                        (if (< a b)
                                            a
                                            b))
                                      (let ((a (* il jl))
                                            (b (* ih jh)))
                                        (if (> a b)
                                            a
                                            b)))))))))

(define (div-interval i j)
  (let ((jl (lower-bound j))
        (jh (upper-bound j)))
    (if (or (zero? jl)
            (zero? jh)
            (and (negative? jl)
                 (positive? jh)))
        (error "div-interval: division bj zero")
        (mul-interval i
                      (make-interval (/ 1.0 jh)
                                     (/ 1.0 jl))))))

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
