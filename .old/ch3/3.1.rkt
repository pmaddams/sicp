#lang sicp

(define (make-accumulator sum)
  (lambda (n)
    (set! sum
          (+ sum n))
    sum))

(define (displayln x)
  (display x)
  (newline))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i)
                       (cons i result))))))
    (e high '())))

(let ((A (make-accumulator 1)))
  (for-each (lambda (n)
              (displayln (A n)))
            (enumerate-interval 2 10)))
;; 3
;; 6
;; 10
;; 15
;; 21
;; 28
;; 36
;; 45
;; 55
