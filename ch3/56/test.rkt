#lang racket/base

(require racket/stream
         rackunit
         "main.rkt")

(test-case
 "hamming-numbers"
 (check-equal? (stream->list (stream-take hamming-numbers 20))
               '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)))
