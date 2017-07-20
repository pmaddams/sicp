#lang sicp

(define (r a b)
  (remainder a b))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (r a b))))

;; normal-order

(gcd 206 40)
(if (zero? 40)
    206
    (gcd 40 (r 206 40)))
(gcd 40 (r 206 40))
(if (zero? (r 206 40))
    40
    (gcd (r 206 40) (r 40 (r 206 40))))
(gcd (r 206 40) (r 40 (r 206 40)))
(if (zero? (r 40 (r 206 40)))
    (r 206 40)
    (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
(gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
(if (zero? (r (r 206 40) (r 40 (r 206 40))))
    (r 40 (r 206 40))
    (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
(gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
(if (zero? (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
    (r (r 206 40) (r 40 (r 206 40)))
    (gcd (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
(r (r 206 40) (r 40 (r 206 40)))
(r 6 (r 40 6))
(r 6 4)
2

;; The remainder procedure is actually applied each time the if special form is
;; used. This means it must be applied 14 times before the procedure has evolved
;; to the point where gcd is no longer part of the expression tree. Then there
;; are 4 more applications of remainder to yield the final result, for a total
;; of 18.

;; applicative-order

(gcd 206 40)
(if (zero? 40)
    206
    (gcd 40 (r 206 40)))
(gcd 40 6)
(if (zero? 6)
    40
    (gcd 6 (r 40 6)))
(gcd 6 4)
(if (zero? 4)
    6
    (gcd 4 (r 6 4)))
(gcd 4 2)
(if (zero? 2)
    4
    (gcd 2 (r 4 2)))
(gcd 2 0)
(if (zero? 0)
    2
    (gcd 0 (r 2 0)))
2

;; The applicative-order interpretation only performs the remainder procedure 4
;; times, since the final else clause is not evaluated.