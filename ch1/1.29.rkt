#lang sicp

(define (sum term a next b)
  (letrec ((s (lambda (a result)
                (if (> a b)
                    result
                    (s (next a)
                       (+ result (term a)))))))
    (s a 0)))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x)
            (+ x dx))
          b)
     dx))

(define (simpson f a b n)
  (let* ((h (/ (- b a) n))
         (move (lambda (steps plus-or-minus)
                 (lambda (x)
                   (plus-or-minus x (* h steps))))))
    (* (/ h 3.0)
       (+ (f a)
          (f b)
          (* 2 (sum f ((move 1 +) a) (move 2 +) ((move 1 -) b)))
          (* 4 (sum f ((move 2 +) a) (move 2 +) ((move 2 -) b)))))))

(define (cube x)
  (expt x 3))

(integral cube 0 1 0.01)
;; 0.24998750000000042

(simpson cube 0 1 100)
;; 0.2467166666666667

(integral cube 0 1 0.001)
;; 0.249999875000001

(simpson cube 0 1 1000)
;; 0.24966716666666663
