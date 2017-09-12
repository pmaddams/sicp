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
                   (not (negative? n)))))
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

;; Let i = [il, ih] and j = [jl, jh].
;;
;; width(i) = (ih - il) / 2
;; center(i) = (il + ih) / 2
;; percent(i) = 100 * width(i) / center(i) = 100 * (ih - il) / (il + ih)
;;
;; For intervals with nonnegative lower bounds,
;;
;; i * j = [il*jl, ih*jh]
;; percent(i*j) = 100 * (ih*jh - il*jl) / (il*jl + ih*jh)
;;
;; Let a = il * jl and b = ih * jh.
;;
;; percent(i*j) = 100 * (b - a) / (a + b)

(define (product-percent i j)
  (let ((a (* (lower-bound i)
              (lower-bound j)))
        (b (* (upper-bound i)
              (upper-bound j))))
    (* 100.0 (/ (- b a) (+ a b)))))

(define (displayln x)
  (display x)
  (newline))

(let ((i (make-interval 0 1))
      (j (make-interval 2 3)))
  (displayln (percent (mul-interval i j)))
  (displayln (product-percent i j)))
;; 100.0
;; 100.0
