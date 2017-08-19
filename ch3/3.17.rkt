#lang sicp

(define (count-pairs x)
  (let ((seen '()))
    (letrec ((c (lambda (x)
                  (if (or (not (pair? x))
                          (memq x seen))
                      0
                      (begin (set! seen (cons x seen))
                             (+ (c (car x))
                                (c (cdr x))
                                1))))))
      (c x))))

(define (displayln x)
  (display x)
  (newline))

(let* ((a '(1))
       (b (cons a a))
       (c (cons b b)))
  (displayln (count-pairs a))
  (displayln (count-pairs b))
  (displayln (count-pairs c)))
;; 1
;; 2
;; 3
