#lang racket/base

; Exercise 2.49

(require racket/class
         racket/draw)

(struct point (x y))

(struct segment (start end))

(struct frame (origin e1 e2))

(define size 300)

(define bitmap (make-bitmap size size))

(define dc (make-object bitmap-dc% bitmap))

(define unit-frame
  (frame (point 0.0 size)
         (point size 0.0)
         (point 0.0 (- size))))

(define (paint painter)
  (painter unit-frame)
  (let ((output bitmap))
    (reset)
    output))

(define (reset)
  (set! bitmap (make-bitmap size size))
  (set! dc (make-object bitmap-dc% bitmap)))

(define (make-painter . segment-coordinates)
  (segments->painter
   (apply append (map make-segment segment-coordinates))))

(define (make-segment list-of-coordinates)
  (points->segments
   (for/list ((l list-of-coordinates))
     (apply point l))))

(define ((segments->painter l) fr)
  (for ((s l))
    (let ((p1 ((coordinate-map fr) (segment-start s)))
          (p2 ((coordinate-map fr) (segment-end s))))
      (send dc draw-line
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
           (point-sub (cm e2) new-origin))))

(define (square-limit painter n)
  (let* ((quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half)))

(define (corner-split painter n)
  (if (zero? n)
      painter
      (let* ((upper (up-split painter (sub1 n)))
             (right (right-split painter (sub1 n)))
             (upper-left (beside upper upper))
             (lower-right (below right right))
             (corner (corner-split painter (sub1 n))))
        (beside (below painter upper-left)
                (below lower-right corner)))))

(define (up-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (up-split painter (sub1 n))))
        (below painter (beside smaller smaller)))))

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
   (point 1.0 0.0)
   (point 0.0 0.0)
   (point 1.0 1.0)))

(define flip-vert
  (transform-painter
   (point 0.0 1.0)
   (point 1.0 1.0)
   (point 0.0 0.0)))

(define rotate90
  (transform-painter
   (point 1.0 0.0)
   (point 1.0 1.0)
   (point 0.0 0.0)))

(define rotate180
  (transform-painter
   (point 1.0 1.0)
   (point 0.0 1.0)
   (point 1.0 0.0)))

(define rotate270
  (transform-painter
   (point 0.0 1.0)
   (point 0.0 0.0)
   (point 1.0 1.0)))

(define wave
  (make-painter
   '((0.00 0.85) (0.15 0.60) (0.30 0.65) (0.40 0.65) (0.35 0.85) (0.40 1.00))
   '((0.60 1.00) (0.65 0.85) (0.60 0.65) (0.75 0.65) (1.00 0.35))
   '((1.00 0.15) (0.60 0.45) (0.75 0.00))
   '((0.60 0.00) (0.50 0.30) (0.40 0.00))
   '((0.25 0.00) (0.35 0.50) (0.30 0.60) (0.15 0.40) (0.00 0.65))))
