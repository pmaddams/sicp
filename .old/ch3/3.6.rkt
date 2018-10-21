#lang sicp

(define (make-rand)
  (let* ((a #xfdeece66d)
         (b #xb)
         (m #x1000000000000)
         (x (runtime))
         (generate (lambda ()
                     (set! x
                           (modulo (+ (* a x) b)
                                   m))
                     x))
         (reset (lambda (y)
                  (set! x y)
                  x))
         (dispatch (lambda (m)
                     (case m
                       ('generate (generate))
                       ('reset reset)
                       (else (error "rand: unknown method:" m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((rand (make-rand)))
  (displayln (rand 'generate))
  ((rand 'reset) 1)
  (displayln (rand 'generate))
  ((rand 'reset) 1)
  (displayln (rand 'generate)))
;; 239020880097748
;; 68164576888
;; 68164576888
