#lang sicp

(define x '(a b))

(define z1 (cons x x))

(define z2 '((a b) a b))

(define (set-to-wow! lol)
  (set-car! (car lol) 'wow)
  lol)

(define (displayln x)
  (display x)
  (newline))

(displayln z1)
;; ((a b) a b)

(set-to-wow! z1)

;; [*|*]
;;  | |
;;  V V
;; [*|*]->[*|/]
;;  |      |
;;  V      V
;; wow     b

(displayln z1)
;; ((wow b) wow b)

(displayln z2)
;; ((a b) a b)

(set-to-wow! z2)

;; [*|*]->[*|*]->[*|/]
;;  |      |      |
;;  |      V      V
;;  |      a      b
;;  V
;; [*|*]->[*|/]
;;  |      |
;;  V      V
;; wow     b

(displayln z2)
;; ((wow b) (a b))
