#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "paint"
 (values (paint wave)
         (paint (rotate90 wave))
         (paint (rotate180 wave))
         (paint (rotate270 wave))
         (paint (flip-horiz wave))
         (paint (flip-vert wave))
         (paint (beside wave (rotate270 wave)))
         (paint (below (rotate180 wave) wave))
         (paint (right-split wave 1))
         (paint (up-split wave 2))
         (paint (corner-split wave 3))
         (paint (square-limit wave 4))))
