#lang racket/base

; Exercise 2.49: Functional geometry

(require racket/class
         racket/draw)

(define size 300)

(define (paint painter)
  (painter unit-frame)
  (show))

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

(define (make-painter . segment-coordinates)
  (segments->painter
   (apply append (map make-segment segment-coordinates))))

(define (make-segment list-of-coordinates)
  (points->segments
   (for/list ((l list-of-coordinates))
     (apply point l))))

(define (transform-painter origin e1 e2)
  (lambda (painter)
    (compose painter (relative-frame origin e1 e2))))

(define lower-left (point 0.0 0.0))

(define bottom (point 0.5 0.0))

(define lower-right (point 1.0 0.0))

(define right (point 1.0 0.5))

(define upper-right (point 1.0 1.0))

(define top (point 0.5 1.0))

(define upper-left (point 0.0 1.0))

(define left (point 0.0 0.5))

(define center (point 0.5 0.5))

(define flip-horiz
  (transform-painter lower-right lower-left upper-right))

(define flip-vert
  (transform-painter upper-left upper-right lower-left))

(define rotate90
  (transform-painter lower-right upper-right lower-left))

(define rotate180
  (transform-painter upper-right upper-left lower-right))

(define rotate270
  (transform-painter upper-left lower-left upper-right))

(define (superimpose painter1 painter2)
  (lambda (fr)
    (painter1 fr)
    (painter2 fr)))

(define (beside painter1 painter2)
  (let ((left-side (transform-painter lower-left bottom upper-left))
        (right-side (transform-painter bottom lower-right top)))
    (superimpose (left-side painter1) (right-side painter2))))

(define (below painter1 painter2)
  (let ((bottom-half (transform-painter lower-left lower-right left))
        (top-half (transform-painter left right upper-left)))
    (superimpose (bottom-half painter1) (top-half painter2))))

(define (right-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (right-split painter (sub1 n))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (up-split painter (sub1 n))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (zero? n)
      painter
      (let* ((top-painter (up-split painter (sub1 n)))
             (right-painter (right-split painter (sub1 n)))
             (upper-left-painter (beside top-painter top-painter))
             (lower-right-painter (below right-painter right-painter))
             (corner-painter (corner-split painter (sub1 n))))
        (beside (below painter upper-left-painter)
                (below lower-right-painter corner-painter)))))

(define (square-limit painter n)
  (let* ((quarter-painter (corner-split painter n))
         (half-painter (beside (flip-horiz quarter-painter) quarter-painter)))
    (below (flip-vert half-painter) half-painter)))

(define wave
  (make-painter
   '((0.00 0.85) (0.15 0.60) (0.30 0.65) (0.40 0.65) (0.35 0.85) (0.40 1.00))
   '((0.60 1.00) (0.65 0.85) (0.60 0.65) (0.75 0.65) (1.00 0.35))
   '((1.00 0.15) (0.60 0.45) (0.75 0.00))
   '((0.60 0.00) (0.50 0.30) (0.40 0.00))
   '((0.25 0.00) (0.35 0.50) (0.30 0.60) (0.15 0.40) (0.00 0.65))))
