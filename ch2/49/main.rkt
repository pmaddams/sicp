#lang racket/base

; Exercise 2.49

(provide (all-defined-out))

(require racket/class
         racket/draw)

(define size 300)

(struct point (x y))

(struct segment (start end))

(struct frame (origin e1 e2 dc))

(define (paint painter)
  (let* ((bitmap (make-bitmap size size))
         (dc (make-object bitmap-dc% bitmap))
         (unit-frame (frame (point 0 0)
                            (point size 0)
                            (point 0 size)
                            dc))
         (canvas ((relative-frame
                   (point 0.01 0.01)
                   (point 0.99 0.01)
                   (point 0.01 0.99))
                  unit-frame)))
    (painter canvas)
    bitmap))

(define (make-painter . args)
  (segments->painter
   (apply append (map make-segment args))))

(define (make-segment coordinate-list)
  (points->segments
   (for/list ((coords (in-list coordinate-list)))
     (apply point coords))))

(define ((segments->painter l) fr)
  (for ((s (in-list l)))
    (let ((p1 ((coordinate-map fr) (segment-start s)))
          (p2 ((coordinate-map fr) (segment-end s))))
      (send (frame-dc fr) draw-line
            (point-x p1) (point-y p1)
            (point-x p2) (point-y p2)))))

(define (points->segments l)
  (if (< (length l) 2)
      (error "not enough points:" l)
      (let loop ((l l) (acc '()))
        (if (null? (cdr l))
            acc
            (loop (cdr l) (cons (segment (car l) (cadr l)) acc))))))

(define ((coordinate-map fr) pt)
  (point-add (frame-origin fr)
             (point-add (point-scale (point-x pt) (frame-e1 fr))
                        (point-scale (point-y pt) (frame-e2 fr)))))

(define (point-add p1 p2)
  (point (+ (point-x p1) (point-x p2))
         (+ (point-y p1) (point-y p2))))

(define (point-sub p1 p2)
  (point (- (point-x p1) (point-x p2))
         (- (point-y p1) (point-y p2))))

(define (point-scale n pt)
  (point (* n (point-x pt))
         (* n (point-y pt))))

(define ((transform-painter origin e1 e2) painter)
  (compose painter (relative-frame origin e1 e2)))

(define ((relative-frame origin e1 e2) fr)
  (let* ((cm (coordinate-map fr))
         (new-origin (cm origin)))
    (frame new-origin
           (point-sub (cm e1) new-origin)
           (point-sub (cm e2) new-origin)
           (frame-dc fr))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below half (flip-vert half))))

(define (corner-split painter n)
  (if (zero? n)
      painter
      (let* ((upper (up-split painter (sub1 n)))
             (right (right-split painter (sub1 n)))
             (upper-left (beside upper upper))
             (lower-right (below right right))
             (corner (corner-split painter (sub1 n))))
        (beside (below upper-left painter)
                (below corner lower-right)))))

(define (up-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (up-split painter (sub1 n))))
        (below (beside smaller smaller) painter))))

(define (right-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (right-split painter (sub1 n))))
        (beside painter (below smaller smaller)))))

(define (beside painter1 painter2)
  (let ((left (transform-painter
               (point 0.0 0.0)
               (point 0.5 0.0)
               (point 0.0 1.0)))
        (right (transform-painter
                (point 0.5 0.0)
                (point 1.0 0.0)
                (point 0.5 1.0))))
    (superimpose (left painter1) (right painter2))))

(define (below painter1 painter2)
  (let ((lower (transform-painter
                (point 0.0 0.0)
                (point 1.0 0.0)
                (point 0.0 0.5)))
        (upper (transform-painter
                (point 0.0 0.5)
                (point 1.0 0.5)
                (point 0.0 1.0))))
    (superimpose (lower painter1) (upper painter2))))

(define ((superimpose painter1 painter2) fr)
  (painter1 fr)
  (painter2 fr))

(define flip-horiz
  (transform-painter
   (point 1 0)
   (point 0 0)
   (point 1 1)))

(define flip-vert
  (transform-painter
   (point 0 1)
   (point 1 1)
   (point 0 0)))

(define rotate90
  (transform-painter
   (point 0 1)
   (point 0 0)
   (point 1 1)))

(define rotate180
  (transform-painter
   (point 1 1)
   (point 0 1)
   (point 1 0)))

(define rotate270
  (transform-painter
   (point 1 0)
   (point 1 1)
   (point 0 0)))

(define outline
  (make-painter
   '((0 0) (1 0) (1 1) (0 1) (0 0))))

(define X
  (make-painter
   '((0 0) (1 1))
   '((1 0) (0 1))))

(define diamond
  (make-painter
   '((0.5 0.0) (1.0 0.5) (0.5 1.0) (0.0 0.5) (0.5 0.0))))

(define wave
  (make-painter
   '((0.00 0.15) (0.15 0.40) (0.30 0.35) (0.40 0.35) (0.35 0.15) (0.40 0.00))
   '((0.60 0.00) (0.65 0.15) (0.60 0.35) (0.75 0.35) (1.00 0.65))
   '((1.00 0.85) (0.60 0.55) (0.75 1.00))
   '((0.60 1.00) (0.50 0.70) (0.40 1.00))
   '((0.25 1.00) (0.35 0.50) (0.30 0.40) (0.15 0.60) (0.00 0.35))))
