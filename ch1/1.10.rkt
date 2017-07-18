#lang sicp

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (dec x)
                 (A x (dec y))))))

(A 1 10)
;; 1024

(A 2 4)
;; 65536

(A 3 3)
;; 65536

(define (f n)
  (A 0 n))

;; (f n)
;; (A 0 n)
;; (cond ((= n 0) 0)
;;       ((= 0 0) (* 2 n))
;;       ((= n 1) 2)
;;       (else (A (dec 0)
;;                (A 0 (dec n)))))
;; (* 2 n)

(define (g n)
  (A 1 n))

;; (g n)
;; (A 1 n)
;; (cond ((= n 0) 0)
;;       ((= 1 0) (* 2 n))
;;       ((= n 1) 2)
;;       (else (A (dec 1)
;;                (A 1 (dec n)))))
;; (A 0 (A 1 (dec n)))
;; (* 2 (A 1 (dec n)))
;; (expt 2 n)

(define (h n)
  (A 2 n))

;; (h n)
;; (A 2 n)
;; (cond ((= n 0) 0)
;;       ((= 2 0) (* 2 n))
;;       ((= n 1) 2)
;;       (else (A (dec 2)
;;                (A 2 (dec n)))))
;; (A 1 (A 2 (dec n)))
;; (expt 2 (A 2 (dec n)))
;; (expt 2 (expt 2 ... )))