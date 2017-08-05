#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)

(define right-branch cadr)

(define branch-length car)

(define branch-structure cadr)

(define (branch-weight b)
  (let ((s (branch-structure b)))
    (if (number? s)
        s
        (total-weight s))))

(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (balanced? m)
  (let ((torque (lambda (b)
                  (* (branch-length b)
                     (branch-weight b))))
        (branch-balanced? (lambda (b)
                            (let ((s (branch-structure b)))
                              (if (number? s)
                                  #t
                                  (balanced? s))))))
    (let ((l (left-branch m))
          (r (right-branch m)))
      (and (= (torque l)
              (torque r))
           (branch-balanced? l)
           (branch-balanced? r)))))

(let ((m (make-mobile (make-branch 4 2)
                      (make-branch 2 (make-mobile (make-branch 1 2)
                                                  (make-branch 1 2))))))
  (balanced? m))
;; #t

(begin (set! make-mobile (lambda (left right)
                           (cons left right)))
       (set! make-branch (lambda (length structure)
                           (cons length structure)))
       (set! right-branch cdr)
       (set! branch-structure cdr))

;; Only the right-branch and branch-structure accessors need to change.

(let ((m (make-mobile (make-branch 4 2)
                      (make-branch 2 (make-mobile (make-branch 1 2)
                                                  (make-branch 1 2))))))
  (balanced? m))
;; #t