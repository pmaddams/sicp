#lang racket/base

(require racket/stream
         rackunit
         "main.rkt")

(test-case
 "ramanujan-numbers"
 (check-equal? (stream->list (stream-take ramanujan-numbers 10))
               '(1729 4104 13832 20683 32832 39312 40033 46683 64232 65728)))
