#lang sicp

(define make-point cons)

(define x-point car)

(define y-point cdr)

(define (print-point p)
  (begin (display "(")
         (display (x-point p))
         (display ",")
         (display (y-point p))
         (display ")")
         (newline)))

(define (distance p1 p2)
  (let* ((x1 (x-point p1))
         (y1 (y-point p1))
         (x2 (x-point p2))
         (y2 (y-point p2)))
    (sqrt (+ (expt (- x2 x1) 2)
             (expt (- y2 y1) 2)))))

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

(define (length-seg seg)
  (distance (start-segment seg)
            (end-segment seg)))

(define make-rect-a cons)

(define base-seg-rect-a car)

(define height-seg-rect-a cdr)

(define (base-rect-a ra)
  (length-seg (base-seg-rect-a ra)))

(define (height-rect-a ra)
  (length-seg (height-seg-rect-a ra)))

(define (make-rect-b p1 p2 p3 p4)
  (list p1 p2 p3 p4))

(define (base-and-height-rect-b rb)
  (let* ((p1 (car rb))
         (p2 (cadr rb))
         (p3 (caddr rb))
         (p4 (cadddr rb))
         (d1 (distance p1 p2))
         (d2 (distance p1 p3))
         (d3 (distance p1 p4))
         (diagonal (max d1 d2 d3)))
    (cond ((= diagonal d1) (cons d2 d3))
          ((= diagonal d2) (cons d1 d3))
          (else (cons d1 d2)))))

(define (base-rect-b rb)
  (car (base-and-height-rect-b rb)))

(define (height-rect-b rb)
  (cdr (base-and-height-rect-b rb)))

(define (perimeter base-rect height-rect r)
  (* 2 (+ (base-rect r)
          (height-rect r))))

(define (area base-rect height-rect r)
  (* (base-rect r)
     (height-rect r)))

(let* ((r (make-rect-a (make-segment (make-point 0 0)
                                     (make-point 3 0))
                       (make-segment (make-point 0 0)
                                     (make-point 0 2))))
       (base-rect base-rect-a)
       (height-rect height-rect-a))
  (begin (display (perimeter base-rect height-rect r))
         (newline)
         (display (area base-rect height-rect r))
         (newline)))
;; 10
;; 6

(let* ((r (make-rect-b (make-point 0 0)
                       (make-point 3 0)
                       (make-point 3 2)
                       (make-point 0 2)))
       (base-rect base-rect-b)
       (height-rect height-rect-b))
  (begin (display (perimeter base-rect height-rect r))
         (newline)
         (display (area base-rect height-rect r))
         (newline)))
;; 10
;; 6