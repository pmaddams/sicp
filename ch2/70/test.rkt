#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "get-a-job"
 (let* ((t (make-huffman-tree get-a-job))
        (bits (encode get-a-job t)))
   (check-equal? (decode bits t) get-a-job)
   (check-equal? (length bits) 84)))
