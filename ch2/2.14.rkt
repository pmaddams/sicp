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
  (display " +/- ")
  (display (percent i))
  (display "%")
  (newline))

(let ((r1 (make-center-percent 1000 30))
      (r2 (make-center-percent 2000 30)))
  (print-center-percent (par1 r1 r2))
  (print-center-percent (par2 r1 r2)))
;; 930.4029304029305 +/- 72.99212598425197%
;; 666.6666666666667 +/- 30.000000000000004%

(let ((i (make-center-percent 3000 1))
      (j (make-center-percent 3000 20)))
  (print-center-percent (div-interval i i))
  (print-center-percent (div-interval i j))
  (print-center-percent (div-interval j i))
  (print-center-percent (div-interval j j)))
;; 1.0002000200020003 +/- 1.9998000199980077%
;; 1.0437500000000002 +/- 20.958083832335337%
;; 1.0021002100210021 +/- 20.958083832335326%
;; 1.0833333333333333 +/- 38.46153846153847%
