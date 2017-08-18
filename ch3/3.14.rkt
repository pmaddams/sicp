#lang sicp

(define (mystery x)
  (letrec ((loop (lambda (x y)
                   (if (null? x)
                       y
                       (let ((temp (cdr x)))
                         (set-cdr! x y)
                         (loop temp x))))))
    (loop x '())))

;; The mystery procedure reverses a list.

(define v '(a b c d))

;; [*|*]->[*|*]->[*|*]->[*|/]
;;  |      |      |      |
;;  V      V      V      V
;;  a      b      c      d

(define w (mystery v))

;; [*|*]->[*|*]->[*|*]->[*|/]
;;  |      |      |      |
;;  V      V      V      V
;;  d      c      b      a

(define (displayln x)
  (display x)
  (newline))

(displayln v)
;; (a)

(displayln w)
;; (d c b a)