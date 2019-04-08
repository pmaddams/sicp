#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "xor-gate"
 (for ((a (in-range 2)))
   (for ((b (in-range 2)))
     (let ((in1 (new wire% (signal a)))
           (in2 (new wire% (signal b)))
           (out (new wire%)))
       (xor-gate in1 in2 out)
       (propagate)
       (check-equal? (get-signal out)
                     (if (not (= a b))
                         1
                         0))
       (reset)))))

(test-case
 "half-adder"
 (for ((a (in-range 2)))
   (for ((b (in-range 2)))
     (let ((in1 (new wire% (signal a)))
           (in2 (new wire% (signal b)))
           (sum (new wire%))
           (carry (new wire%)))
       (half-adder in1 in2 sum carry)
       (propagate)
       (check-equal? (get-signal sum)
                     (if (not (= a b))
                         1
                         0))
       (check-equal? (get-signal carry)
                     (if (and (= a 1) (= b 1))
                         1
                         0))
       (reset)))))

(test-case
 "full-adder"
 (for ((a (in-range 2)))
   (for ((b (in-range 2)))
     (for ((c (in-range 2)))
       (let ((in1 (new wire% (signal a)))
             (in2 (new wire% (signal b)))
             (carry-in (new wire% (signal c)))
             (sum (new wire%))
             (carry-out (new wire%)))
         (full-adder in1 in2 carry-in sum carry-out)
         (propagate)
         (check-equal? (get-signal sum)
                       (if (or (and (= a 0) (= b 0) (= c 1))
                               (and (= a 0) (= b 1) (= c 0))
                               (and (= a 1) (= b 0) (= c 0))
                               (and (= a 1) (= b 1) (= c 1)))
                           1
                           0))
         (check-equal? (get-signal carry-out)
                       (if (or (and (= a 0) (= b 1) (= c 1))
                               (and (= a 1) (= b 0) (= c 1))
                               (and (= a 1) (= b 1) (= c 0))
                               (and (= a 1) (= b 1) (= c 1)))
                           1
                           0))
         (reset))))))
