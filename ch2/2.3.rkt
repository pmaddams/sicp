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

(define (square x)
  (expt x 2))

(define (length-segment seg)
  (let* ((start (start-segment seg))
         (end (end-segment seg)))
    (sqrt (+ (square (- (x-point start)
                        (x-point end)))
             (square (- (y-point start)
                        (y-point end)))))))

(define (base rect)
  (rect 'base))

(define (height rect)
  (rect 'height))

(define (perimeter rect)
  (+ (* 2 (base rect))
     (* 2 (height rect))))

(define (area rect)
  (* (base rect)
     (height rect)))

(define (make-rect-a p1 p2 p3 p4)
  (let* ((base (lambda ()
                 (length-segment
                  (make-segment p1 p2))))
         (height (lambda ()
                   (let ((l1 (length-segment
                              (make-segment p2 p3)))
                         (l2 (length-segment
                              (make-segment p3 p4))))
                     (if (< l1 l2)
                         l1
                         l2))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'base) (base))
                           ((eq? m 'height) (height))
                           (else (error "rect-a: undefined:" m))))))
    dispatch))

(define (make-rect-b seg1 seg2)
  (let* ((base (lambda ()
                 (length-segment seg1)))
         (height (lambda ()
                   (length-segment seg2)))
         (dispatch (lambda (m)
                     (cond ((eq? m 'base) (base))
                           ((eq? m 'height) (height))
                           (else (error "rect-b: undefined:" m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((rect (make-rect-a (make-point 0 0)
                         (make-point 10 0)
                         (make-point 10 10)
                         (make-point 0 10))))
  (displayln (perimeter rect))
  (displayln (area rect)))
;; 40
;; 100

(let ((rect (make-rect-b (make-segment (make-point 0 0)
                                       (make-point 10 0))
                         (make-segment (make-point 0 0)
                                       (make-point 0 10)))))
  (displayln (perimeter rect))
  (displayln (area rect)))
;; 40
;; 100
