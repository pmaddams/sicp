#lang sicp

(define (factorial-a n)
  (if (<= n 1)
      1
      (* n (factorial-a (dec n)))))

(factorial-a 6)
;; 720

;;             -------------
;; global-env->|factorial-a|
;;             -----|-------
;;                  V
;;               ------
;;           E1->|n: 6|
;;               ------
;;           (if (<= n 1)
;;               1
;;               (* n (factorial-a (dec n))))

(define (factorial-b n)
  (letrec ((f (lambda (result counter)
                (if (> counter n)
                    result
                    (f (* result counter)
                       (inc counter))))))
    (f 1 1)))

(factorial-b 6)
;; 720

;;             -------------
;; global-env->|factorial-b|
;;             -----|-------
;;                  V
;;               ------
;;           E1->|n: 6|
;;               |f   |
;;               -|----
;;                V  (f 1 1)
;;               ------------
;;           E2->|result: 1 |
;;               |counter: 1|
;;               ------------
;;           (if (> counter n)
;;               result
;;               (f (* result counter)
;;                  (inc counter)))