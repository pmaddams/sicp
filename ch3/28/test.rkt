#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "xor-gate"
 (for ((i (in-range 5)))
   (let ((in1 (new wire% (signal (random 2))))
         (in2 (new wire% (signal (random 2))))
         (out (new wire%)))
     (xor-gate in1 in2 out)
     (propagate)
     (check-equal? (get-signal out)
                   (if (not (= (get-signal in1) (get-signal in2)))
                       1
                       0))
     (reset))))

(test-case
 "half-adder"
 (for ((i (in-range 5)))
   (let ((in1 (new wire% (signal (random 2))))
         (in2 (new wire% (signal (random 2))))
         (sum (new wire%))
         (carry (new wire%)))
     (half-adder in1 in2 sum carry)
     (propagate)
     (check-equal? (get-signal sum)
                   (if (not (= (get-signal in1) (get-signal in2)))
                       1
                       0))
     (check-equal? (get-signal carry)
                   (if (and (= (get-signal in1) 1)
                            (= (get-signal in2) 1))
                       1
                       0))
     (reset))))

(test-case
 "full-adder"
 (for ((i (in-range 5)))
   (let ((in1 (new wire% (signal (random 2))))
         (in2 (new wire% (signal (random 2))))
         (carry-in (new wire% (signal (random 2))))
         (sum (new wire%))
         (carry-out (new wire%)))
     (full-adder in1 in2 carry-in sum carry-out)
     (propagate)
     (check-equal? (get-signal sum)
                   (if (or (and (= (get-signal in1) 0)
                                (= (get-signal in2) 0)
                                (= (get-signal carry-in) 1))
                           (and (= (get-signal in1) 0)
                                (= (get-signal in2) 1)
                                (= (get-signal carry-in) 0))
                           (and (= (get-signal in1) 1)
                                (= (get-signal in2) 0)
                                (= (get-signal carry-in) 0))
                           (and (= (get-signal in1) 1)
                                (= (get-signal in2) 1)
                                (= (get-signal carry-in) 1)))
                       1
                       0))
     (check-equal? (get-signal carry-out)
                   (if (or (and (= (get-signal in1) 0)
                                (= (get-signal in2) 1)
                                (= (get-signal carry-in) 1))
                           (and (= (get-signal in1) 1)
                                (= (get-signal in2) 0)
                                (= (get-signal carry-in) 1))
                           (and (= (get-signal in1) 1)
                                (= (get-signal in2) 1)
                                (= (get-signal carry-in) 0))
                           (and (= (get-signal in1) 1)
                                (= (get-signal in2) 1)
                                (= (get-signal carry-in) 1)))
                       1
                       0))
     (reset))))
