#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "paint"
 (values (paint outline)
         (paint X)
         (paint diamond)
         (paint wave)
         (paint (rotate90 wave))
         (paint (rotate180 wave))
         (paint (rotate270 wave))
         (paint (flip-horiz wave))
         (paint (flip-vert wave))
         (paint (superimpose X (beside wave (rotate270 wave))))
         (paint (let ((p (superimpose outline wave)))
                  (below (rotate180 p) p)))
         (paint (up-split wave 3))
         (paint (let ((p (superimpose diamond X)))
                  (right-split p 3)))
         (paint (corner-split (superimpose outline diamond) 5))
         (paint (square-limit wave 4))))
