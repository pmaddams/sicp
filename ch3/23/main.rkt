#lang racket/base

; Exercise 3.23: Deques

(require racket/class)

(define deque%
  (class object%
    (super-new)

    (struct node (val prev next) #:mutable)

    (field (front '()) (back '()))

    (define/public (push-front val)
      (push val 'front))

    (define/public (pop-front)
      (pop 'front))

    (define/public (push-back val)
      (push val 'back))

    (define/public (pop-back)
      (pop 'back))

    (define/public (empty?)
      (let ((fail (lambda () (error "invalid state:" front back))))
        (if (null? front)
            (or (null? back) (fail))
            (and (null? back) (fail)))))

    (define (push val where)
      (let ((elem (node val '() '())))
        (cond ((empty?) (set! front elem)
                        (set! back elem))
              ((eq? where 'front) (set-node-next! elem front)
                                  (set-node-prev! front elem)
                                  (set! front elem))
              ((eq? where 'back) (set-node-prev! elem back)
                                 (set-node-next! back elem)
                                 (set! back elem)))))

    (define (pop where)
      (cond ((empty?) (error "empty deque"))
            ((eq? where 'front) (let ((val (node-val front)))
                                  (set! front (node-next front))
                                  (if (null? front)
                                      (set! back '())
                                      (set-node-prev! front '()))
                                  val))
            ((eq? where 'back) (let ((val (node-val back)))
                                 (set! back (node-prev back))
                                 (if (null? back)
                                     (set! front '())
                                     (set-node-next! back '()))
                                 val))))))
