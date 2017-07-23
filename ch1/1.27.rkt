#lang sicp

(define (expmod base exp m)
  (letrec ((square (lambda (x)
                     (expt x 2)))
           (e (lambda (exp)
                (cond ((zero? exp) 1)
                      ((even? exp) (remainder (square (e (/ exp 2)))
                                              m))
                      (else (remainder (* base (e (dec exp)))
                                       m))))))
    (e exp)))

(define (fools? n)
  (letrec ((f (lambda (a)
             (if (= a n)
                 #t
                 (and (= a (expmod a n n))
                      (f (inc a)))))))
    (f 1)))

(let ((carmichael '(561 1105 1729 2465 2821 6601)))
  (for-each (lambda (n)
              (begin (display n)
                     (display (if (fools? n)
                                  " fools "
                                  " doesn't fool "))
                     (display "the Fermat test.")
                     (newline)))
            carmichael))