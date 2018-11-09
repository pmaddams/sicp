#lang racket/base

; Exercise 3.30: Circuit simulator

(require racket/class)

(define wire%
  (class object%
    (super-new)

    (field (signal 0) (actions '()))

    (define/public (set val)
      (unless (or (= val 0) (= val 1))
        (error "invalid signal:" val))
      (unless (= val signal)
        (set! signal val)
        (for ((action actions))
          (action))))

    (define/public (add action)
      (set! actions (cons action actions))
      (action))))

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

    (field (current-time 0) (segments '()))

    (define/public (propagate)
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

(define (after delay action)
  (send agenda after delay action))

(define (current-time)
  (get-field current-time agenda))

(define (set wire val)
  (send wire set val))

(define (add wire action)
  (send wire add action))

(define (receive wire)
  (get-field signal wire))

(define (logical-not a)
  (cond ((= a 0) 1)
        ((= a 1) 0)
        (else (error "invalid input:" a))))

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

(define-syntax-rule (perform expr ...)
  (lambda () expr ...))

(define (probe name wire)
  (send wire add
        (perform
         (printf "~a -- current time: ~a new value: ~a\n"
                 name (current-time) (receive wire)))))

(define (inverter in out)
  (define delay 2)
  (define action
    (perform
     (let ((val (logical-not (receive in))))
       (after delay (perform (set out val))))))
  (add in action))

(define (nand-gate in1 in2 out)
  (define delay 3)
  (define action
    (perform
     (let ((val (logical-not
                 (logical-and (receive in1)
                              (receive in2)))))
       (after delay (perform (set out val))))))
  (add in1 action)
  (add in2 action))

(define (nor-gate in1 in2 out)
  (define delay 3)
  (define action
    (perform
     (let ((val (logical-not
                 (logical-or (receive in1)
                             (receive in2)))))
       (after delay (perform (set out val))))))
  (add in1 action)
  (add in2 action))

(define-syntax-rule (circuit (wire ...) expr ...)
  (let ((wire (new wire%)) ...)
    expr ...))

(define (xor-gate in1 in2 out)
  (circuit (a b c)
           (nand-gate in1 in2 a)
           (nand-gate in1 a b)
           (nand-gate in2 a c)
           (nand-gate b c out)))

(define (half-adder in1 in2 sum carry)
  (circuit (a)
           (xor-gate in1 in2 sum)
           (nand-gate in1 in2 a)
           (inverter a carry)))

(define (full-adder in1 in2 carry-in sum carry-out)
  (circuit (a b c d)
           (half-adder in1 in2 a b)
           (half-adder carry-in a sum c)
           (nor-gate b c d)
           (inverter d carry-out)))
