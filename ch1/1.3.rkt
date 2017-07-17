#lang sicp

(define (sum-of-two-larger-squares x y z)
  (call-with-values
   (lambda ()
     (let* ((smallest (min x y z))
            (larger-two (lambda (x y z)
                          (cond ((= x smallest) (values y z))
                                ((= y smallest) (values x z))
                                (else (values x y))))))
       (larger-two x y z)))
   (lambda (x y)
     (let ((square (lambda (x)
                     (expt x 2))))
       (+ (square x)
          (square y))))))