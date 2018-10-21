#lang sicp

(define (same-parity first . rest)
  (let ((same? (if (odd? first)
                   odd?
                   even?)))
    (letrec ((s (lambda (rest result)
                  (cond ((null? rest)
                         result)
                        ((same? (car rest))
                         (s (cdr rest)
                            (append result (list (car rest)))))
                        (else
                         (s (cdr rest)
                            result))))))
      (cons first (s rest '())))))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln
          (map (lambda (x)
                 (apply same-parity x))
               '((1 2 3 4 5 6 7)
                 (2 3 4 5 6 7))))
;; (1 3 5 7)
;; (2 4 6)
