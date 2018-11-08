#lang racket/base

; Exercise 3.30: Circuit simulator

(require racket/class)

(define wire%
  (class object%
    (super-new)

    (field (actions '()) (signal 0))

    (define/public (add action)
      (set! actions (cons action actions))
      (action))

    (define/public (set val)
      (unless (or (= val 0) (= val 1))
        (error "invalid signal:" val))
      (unless (= signal val)
        (set! signal val)
        (for ((action actions))
          (action))))))

(define queue%
  (class object%
    (super-new)

    (struct node (val next) #:mutable)

    (field (front '()) (back '()))

    (define/public (push val)
      (let ((elem (node val '())))
        (if (empty?)
            (set! front elem)
            (set-node-next! back elem))
        (set! back elem)))

    (define/public (pop)
      (if (empty?)
          (error "empty queue")
          (let ((val (node-val front)))
            (set! front (node-next front))
            val)))

    (define/public (empty?)
      (null? front))))
