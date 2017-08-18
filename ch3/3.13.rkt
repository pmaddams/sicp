#lang sicp

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (make-cycle l)
  (set-cdr! (last-pair l) l)
  l)

(define z (make-cycle '(a b c)))

;;  -----------------
;;  |               |
;;  V               |
;; [*|*]->[*|*]->[*|*]
;;  |      |      |
;;  V      V      V
;;  a      b      c

(display z)
;; #0=(a b c . #0#)