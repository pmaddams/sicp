#lang sicp

;; note: rectangle may have strange orientation. revise.

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

(define (length-seg seg)
  (let* ((start (start-segment seg))
         (x-start (x-point start))
         (y-start (y-point start))
         (end (end-segment seg))
         (x-end (x-point end))
         (y-end (y-point end)))
    (sqrt (+ (expt (- x-end x-start) 2)
             (expt (- y-end y-start) 2)))))

(define (make-rect-a p1 p2)
  (cons p1 p2))

(define (first-point ra)
  (car ra))

(define (second-point ra)
  (cdr ra))

(define (x-dimension-a ra)
  (let* ((x1 (x-point (first-point ra)))
         (x2 (x-point (second-point ra))))
    (abs (- x2 x1))))

(define (y-dimension-a ra)
  (let* ((y1 (y-point (first-point ra)))
         (y2 (y-point (second-point ra))))
    (abs (- y2 y1))))

(define (make-rect-b hs vs)
  (cons hs vs))

(define (h-seg rb)
  (car rb))

(define (v-seg rb)
  (cdr rb))

(define (x-dimension-b rb)
  (length-seg (h-seg rb)))

(define (y-dimension-b rb)
  (length-seg (v-seg rb)))

(define (perimeter x-dimension y-dimension r)
  (* 2 (+ (x-dimension r)
          (y-dimension r))))

(define (area x-dimension y-dimension r)
  (* (x-dimension r)
     (y-dimension r)))