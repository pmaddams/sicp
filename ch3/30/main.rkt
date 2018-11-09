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

(define segment%
  (class queue%
    (super-new)

    (init action)
    (init-field time)

    (send this push action)))

(define agenda%
  (class object%
    (super-new)

    (field (segments '()) (current-time 0))

    (define/public (run)
      (for ((segment segments))
        (set! current-time (get-field time segment))
        (let loop ((segment segment))
          (unless (send segment empty?)
            ((send segment pop))
            (loop segment))))
      (set! segments '()))

    (define/public (after delay action)
      (let ((time (+ current-time delay)))
        (if (before? time segments)
            (set! segments (mcons (make-object segment% action time) segments))
            (let loop ((segments segments))
              (let ((first (mcar segments))
                    (rest (mcdr segments)))
                (cond ((= (get-field time first) time) (send first push action))
                      ((before? time rest) (set-mcdr! segments (mcons (make-object segment% action time) rest)))
                      (else (loop rest))))))))

    (define (before? time segments)
      (or (null? segments)
          (< time (get-field time (mcar segments)))))))
