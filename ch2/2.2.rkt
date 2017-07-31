#lang sicp

(define (average . args)
  (/ (apply + args)
     (length args)))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (begin (display "(")
         (display (x-point p))
         (display ",")
         (display (y-point p))
         (display ")")
         (newline)))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-seg seg)
  (let* ((start (start-segment seg))
         (x-start (x-point start))
         (y-start (y-point start))
         (end (end-segment seg))
         (x-end (x-point end))
         (y-end (y-point end)))
    (make-point (average x-start x-end)
                (average y-start y-end))))

(let ((seg (make-segment (make-point 0 0)
                         (make-point 10 10))))
  (print-point (midpoint-seg seg)))
;; (5,5)