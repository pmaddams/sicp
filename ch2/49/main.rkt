#lang racket/base

; Exercise 2.49: Functional geometry

(require racket/class
         racket/draw)

(define size 300)

(define (paint painter)
  (painter unit-frame)
  (show))

(define (make-painter . segment-coordinates)
  (segments->painter
   (apply append (map make-segment segment-coordinates))))

(define (make-segment list-of-coordinates)
  (points->segments
   (for/list ((l list-of-coordinates))
     (apply point l))))

(define (show) bitmap)

(define bitmap (make-bitmap size size))

(define dc (make-object bitmap-dc% bitmap))

(define (reset)
  (set! bitmap (make-bitmap size size))
  (set! dc (make-object bitmap-dc% bitmap)))

(struct point (x y))

(define (point-add p1 p2)
  (point (+ (point-x p1) (point-x p2))
         (+ (point-y p1) (point-y p2))))

(define (point-sub p1 p2)
  (point (- (point-x p1) (point-x p2))
         (- (point-y p1) (point-y p2))))

(define (point-scale n pt)
  (point (* n (point-x pt))
         (* n (point-y pt))))

(struct segment (start end))

(define (segments->painter l)
  (lambda (fr)
    (for ((seg l))
      (let ((p1 ((coordinate-map fr) (segment-start seg)))
            (p2 ((coordinate-map fr) (segment-end seg))))
        (send dc draw-line (point-x p1) (point-y p1) (point-x p2) (point-y p2))))))

(define (points->segments l)
  (if (< (length l) 2)
      (error "not enough points:" l)
      (let loop ((l l) (acc '()))
        (if (null? (cdr l))
            acc
            (loop (cdr l) (cons (segment (car l) (cadr l)) acc))))))

(struct frame (origin e1 e2))

(define unit-frame
  (frame (point 0 size)
         (point size 0)
         (point 0 (- size))))

(define (relative-frame origin e1 e2)
  (lambda (fr)
    (let* ((cm (coordinate-map fr))
           (new-origin (cm origin)))
      (frame new-origin
             (point-sub (cm e1) new-origin)
             (point-sub (cm e2) new-origin)))))

(define (coordinate-map fr)
  (lambda (pt)
    (point-add (frame-origin fr)
               (point-add (point-scale (point-x pt) (frame-e1 fr))
                          (point-scale (point-y pt) (frame-e2 fr))))))

(define wave
  (make-painter
   '((0.00 0.85) (0.15 0.60) (0.30 0.65) (0.40 0.65) (0.35 0.85) (0.40 1.00))
   '((0.60 1.00) (0.65 0.85) (0.60 0.65) (0.75 0.65) (1.00 0.35))
   '((1.00 0.15) (0.60 0.45) (0.75 0.00))
   '((0.60 0.00) (0.50 0.30) (0.40 0.00))
   '((0.25 0.00) (0.35 0.50) (0.30 0.60) (0.15 0.40) (0.00 0.65))))
