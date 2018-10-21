#lang sicp

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define a (cons 1 '()))

(define b (cons 2 a))

(define c (cons 3 b))

(count-pairs c)
;; 3

;; [*|*]->[*|*]->[*|/]
;;  |      |      |
;;  V      V      V
;;  3      2      1
 
(define d (cons a a))

(define e (cons 4 d))

(count-pairs e)
;; 4

;; [*|*]->[*|*]
;;  |      \ /
;;  V       |
;;  4       V
;;        [*|/]
;;         |
;;         V
;;         1

(define f (cons d d))

(count-pairs f)
;; 7

;; [*|*]
;;  \ /
;;   |
;;   V
;; [*|*]
;;  \ /
;;   |
;;   V
;; [*|/]
;;  |
;;  V
;;  1

(set-cdr! a f)

;; (count-pairs f)
;; <does not terminate>

;; [*|*]<-
;;  \ /  |
;;   |   |
;;   V   |
;; [*|*] |
;;  \ /  |
;;   |   |
;;   V   |
;; [*|*]--
;;  |
;;  V
;;  1
