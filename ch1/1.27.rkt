#lang sicp

(define (square x)
  (expt x 2))

(define (expmod a n m)
  (letrec ((e (lambda (n)
                (cond ((zero? n)
                       1)
                      ((even? n)
                       (remainder (square (e (/ n 2)))
                                  m))
                      (else
                       (remainder (* a
                                     (e (dec n)))
                                  m))))))
    (e n)))

(define (fools? n)
  (letrec ((f (lambda (a)
                (or (= a n)
                    (and (= a (expmod a n n))
                         (f (inc a)))))))
    (f 1)))

(let ((carmichael '(561 1105 1729 2465 2821 6601)))
  (for-each (lambda (n)
              (display n)
              (display (if (fools? n)
                           " fools "
                           " doesn't fool "))
              (display "the Fermat test.")
              (newline))
            carmichael))
;; 561 fools the Fermat test.
;; 1105 fools the Fermat test.
;; 1729 fools the Fermat test.
;; 2465 fools the Fermat test.
;; 2821 fools the Fermat test.
;; 6601 fools the Fermat test.
