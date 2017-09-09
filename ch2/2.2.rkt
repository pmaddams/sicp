#lang sicp

(define (average . args)
  (/ (apply + args)
     (length args)))

(define make-point cons)

(define x-point car)

(define y-point cdr)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

(define (midpoint-segment seg)
  (let* ((start (start-segment seg))
         (end (end-segment seg)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

(let ((seg (make-segment (make-point 0 0)
                         (make-point 10 10))))
  (print-point (midpoint-segment seg)))
;; (5, 5)
