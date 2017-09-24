#lang sicp

(define rand
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
                  (set! x y)))
         (dispatch (lambda (m)
                     (case m
                       ('generate (generate))
                       ('reset reset)
                       (else (error "rand: unknown method:" m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(displayln (rand 'generate))
((rand 'reset) 1)
(displayln (rand 'generate))
((rand 'reset) 1)
(displayln (rand 'generate))
;; 207885167744470
;; 68164576888
;; 68164576888
