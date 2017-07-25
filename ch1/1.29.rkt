#lang sicp

(define (cube x) (expt x 3))

(define (sum term a next b)
  (letrec ((s (lambda (a)
                (if (> a b)
                    0
                    (+ (term a)
                       (s (next a)))))))
    (s a)))

(define (integral f a b dx)
  (let ((add-dx (lambda (x)
                  (+ x dx))))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx)))

(define (simpson-integral f a b n)
  (let* ((h (/ (- b a) n))
         (move (lambda (steps plus-or-minus)
                 (lambda (x)
                   (plus-or-minus x (* h steps))))))
    (* (/ h 3.0)
       (+ (f a)
          (f b)
          (* 2 (sum f ((move 1 +) a) (move 2 +) ((move 1 -) b)))
          (* 4 (sum f ((move 2 +) a) (move 2 +) ((move 2 -) b)))))))

(integral cube 0 1 0.01)
;; 0.24998750000000042

(simpson-integral cube 0 1 100)
;; 0.2467166666666667

(integral cube 0 1 0.001)
;; 0.249999875000001

(simpson-integral cube 0 1 1000)
;; 0.24966716666666663