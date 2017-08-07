#lang sicp

(define (subsets s)
  (if (null? s)
      '(())
      (let ((rest (subsets (cdr s)))
            (p (lambda (x)
                 (cons (car s) x))))
        (append rest (map p rest)))))

;; The base case for this procedure results from taking the empty list as an
;; argument, which happens once we have traversed the list to the end. In this
;; case we return the empty set. Traveling up the stack of recursive procedures,
;; we now have a list of one element. The procedure which we map to rest
;; consists of attaching this element to all the elements in rest, and appending
;; the results to become the new rest variable. As we continue traveling up the
;; stack, this procedure retains the results gathered so far, and appends the
;; new list of each successive element attached to those results.
