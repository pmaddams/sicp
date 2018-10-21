#lang sicp

(letrec ((+ (lambda (a b)
              (if (zero? a)
                  b
                  (inc (+ (dec a) b))))))
  (+ 4 5))

;; (+ 4 5)
;; (if (zero? 4)
;;     5
;;     (inc (+ (dec 4) 5)))
;; (inc (+ 3 5))
;; (inc (if (zero? 3)
;;          5
;;          (inc (+ (dec 3) 5))))
;; (inc (inc (+ 2 5)))
;; (inc (inc (if (zero? 2)
;;               5
;;               (inc (+ (dec 2) 5)))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (if (zero? 1)
;;                    5
;;                    (inc (+ (dec 1) 5))))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc (if (zero? 0)
;;                         5
;;                         (inc (+ (dec 0) 5)))))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; The evolution of the process is recursive.

(letrec ((+ (lambda (a b)
              (if (zero? a)
                  b
                  (+ (dec a) (inc b))))))
  (+ 4 5))

;; (+ 4 5)
;; (if (zero? 4)
;;     5
;;     (+ (dec 4) (inc 5)))
;; (+ 3 6)
;; (if (zero? 3)
;;     6
;;     (+ (dec 3) (inc 6)))
;; (+ 2 7)
;; (if (zero? 2)
;;     7
;;     (+ (dec 2) (inc 7)))
;; (+ 1 8)
;; (if (zero? 1)
;;     8
;;     (+ (dec 1) (inc 8)))
;; (+ 0 9)
;; (if (zero? 0)
;;     9
;;     (+ (dec 0) (inc 9)))
;; 9

;; The evolution of the process is iterative.
