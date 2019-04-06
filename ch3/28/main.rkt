#lang racket/base

; Exercise 3.28

(provide (all-defined-out))

(require racket/class
         racket/function
         racket/list)

(struct message (name time signal) #:transparent)

(define queue%
  (class object%
    (super-new)

    (field (front '()) (back '()))

    (define/public (push val)
      (let ((elem (mcons val '())))
        (if (empty?)
            (set! front elem)
            (set-mcdr! back elem))
        (set! back elem)))

    (define/public (pop)
      (if (empty?)
          (error "empty queue")
          (let ((val (mcar front)))
            (set! front (mcdr front))
            val)))

    (define/public (empty?)
      (null? front))))

(define segment%
  (class queue%
    (super-new)

    (init action)
    (init-field time)

    (send this push action)))

(define wire%
  (class object%
    (super-new)

    (init-field (signal 0))
    (field (actions '()))

    (define/public (set new-signal)
      (unless (or (= new-signal 0) (= new-signal 1))
        (error "invalid signal:" new-signal))
      (unless (= signal new-signal)
        (set! signal new-signal)
        (for ((action (in-list actions)))
          (action))))

    (define/public (add action)
      (set! actions (cons action actions))
      (action))))

(define (get-signal wire)
  (get-field signal wire))

(define agenda%
  (class object%
    (super-new)

    (field (current-time 0) (segments '()))

    (define/public (propagate)
      (for ((segment (in-mlist segments)))
        (set! current-time (get-field time segment))
        (let loop ()
          (unless (send segment empty?)
            ((send segment pop))
            (loop))))
      (set! segments '()))

    (define/public (after delay action)
      (let ((time (+ delay current-time)))
        (if (before? time segments)
            (set! segments (mcons (at time action) segments))
            (let loop ((segments segments))
              (let ((first (mcar segments))
                    (rest (mcdr segments)))
                (cond ((= (get-field time first) time) (send first push action))
                      ((before? time rest) (set-mcdr! segments (mcons (at time action) rest)))
                      (else (loop rest))))))))

    (define (before? time segments)
      (or (null? segments)
          (< time (get-field time (mcar segments)))))

    (define (at time action)
      (new segment% (time time) (action action)))))

(define agenda (new agenda%))

(define (propagate)
  (send agenda propagate))

(define (get-time)
  (get-field current-time agenda))

(define (reset)
  (set! agenda (new agenda%)))

(define (probe name wire)
  (send wire add
        (thunk
         (printf "~a -- current time: ~a signal: ~a\n"
                 name (get-time) (get-signal wire)))))

(define-syntax-rule (circuit (wire ...) expr ...)
  (let ((wire (new wire%)) ...)
    expr ...))

(define (full-adder in1 in2 carry-in sum carry-out)
  (circuit (a b c d)
           (half-adder in1 in2 a b)
           (half-adder carry-in a sum c)
           (nor-gate b c d)
           (inverter d carry-out)))

(define (half-adder in1 in2 sum carry)
  (circuit (a)
           (xor-gate in1 in2 sum)
           (nand-gate in1 in2 a)
           (inverter a carry)))

(define (xor-gate in1 in2 out)
  (circuit (a b c)
           (nand-gate in1 in2 a)
           (nand-gate in1 a b)
           (nand-gate in2 a c)
           (nand-gate b c out)))

(define (nand-gate in1 in2 out)
  (let* ((delay 3)
         (action
          (thunk
           (let ((signal (logical-not
                          (logical-and (get-signal in1)
                                       (get-signal in2)))))
             (send agenda after delay (thunk (send out set signal)))))))
    (send in1 add action)
    (send in2 add action)))

(define (nor-gate in1 in2 out)
  (let* ((delay 3)
         (action
          (thunk
           (let ((signal (logical-not
                          (logical-or (get-signal in1)
                                      (get-signal in2)))))
             (send agenda after delay (thunk (send out set signal)))))))
    (send in1 add action)
    (send in2 add action)))

(define (inverter in out)
  (let* ((delay 2)
         (action
          (thunk
           (let ((signal (logical-not (get-signal in))))
             (send agenda after delay (thunk (send out set signal)))))))
    (send in add action)))

(define (logical-and a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 0)
        ((and (= a 1) (= b 0)) 0)
        ((and (= a 1) (= b 1)) 1)
        (else (error "invalid input:" a b))))

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 1)
        ((and (= a 1) (= b 0)) 1)
        ((and (= a 1) (= b 1)) 1)
        (else (error "invalid input:" a b))))

(define (logical-not a)
  (cond ((= a 0) 1)
        ((= a 1) 0)
        (else (error "invalid input:" a))))
