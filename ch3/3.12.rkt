#lang sicp

(define (append! x y)
  (letrec ((last-pair (lambda (l)
                        (if (null? (cdr l))
                            l
                            (last-pair (cdr l))))))
    (set-cdr! (last-pair x) y)
    x))

(define (displayln x)
  (display x)
  (newline))

(define x '(a b))

(define y '(c d))

(define z (append x y))

(displayln z)
;; (a b c d)

(displayln (cdr x))
;; (b)

;; <car>  <cdr>
;; [*|*]->[*|/]
;;  |      |
;;  V      V
;;  a      b

(define w (append! x y))

(displayln w)
;; (a b c d)

(displayln (cdr x))
;; (b c d)

;; <car>  <cdr>
;; [*|*]->[*|*]->[*|*]->[*|/]
;;  |      |      |      |
;;  V      V      V      V
;;  a      b      c      d