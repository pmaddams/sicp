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

(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]")
  (newline))

(for-each print-interval
          (map (lambda (i)
                 (apply
                  (lambda (a b c d)
                    (mul-interval (make-interval a b)
                                  (make-interval c d)))
                  i))
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
