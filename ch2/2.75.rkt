#lang sicp

(define (make-from-mag-ang r a)
  (let ((dispatch (lambda (m)
                    (case m
                      ('real-part (* r (cos a)))
                      ('imag-part (* r (sin a)))
                      ('magnitude r)
                      ('angle a)
                      (else (error "make-from-mag-ang: unknown method:" m))))))
    dispatch))

(define (displayln x)
  (display x)
  (newline))

(let ((z (make-from-mag-ang 1 0)))
  (for-each displayln
            (list (z 'real-part)
                  (z 'imag-part)
                  (z 'magnitude)
                  (z 'angle))))
;; 1
;; 0
;; 1
;; 0
