#lang sicp

(define (make-from-mag-ang r a)
  (let ((dispatch (lambda (m)
                    (cond ((eq? m 'real-part) (* r (cos a)))
                          ((eq? m 'imag-part) (* r (sin a)))
                          ((eq? m 'magnitude) r)
                          ((eq? m 'angle) a)
                          (else (error "make-from-mag-ang: undefined operation:" m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((z (make-from-mag-ang 1 0)))
  (displayln (z 'real-part))
  (displayln (z 'imag-part))
  (displayln (z 'magnitude))
  (displayln (z 'angle)))
;; 1
;; 0
;; 1
;; 0
