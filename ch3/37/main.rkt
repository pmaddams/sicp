#lang racket/base

; Exercise 3.37: Constraint propagation

(require racket/class)

(define connector%
  (class object%
    (super-new)

    (field (val #f)
           (informant #f)
           (constraints '()))

    (define/public (set?)
      (if informant #t #f))

    (define/public (set new-val setter)
      (if (set?)
          (unless (= val new-val)
            (error "contradiction:" val new-val))
          (begin (set! val new-val)
                 (set! informant setter)
                 (for ((constraint constraints))
                   (unless (or (eq? constraint setter)
                               (eq? constraint 'user))
                     (send constraint inform))))))

    (define/public (forget retractor)
      (when (eq? informant retractor)
        (begin (set! informant #f)
               (for ((constraint constraints))
                 (unless (eq? constraint retractor)
                   (send constraint retract))))))

    (define/public (connect constraint)
      (unless (member constraint constraints)
        (set! constraints (cons constraint constraints)))
      (when (set?)
        (send constraint inform)))))

(define (set? connector)
  (send connector set?))

(define (get connector)
  (get-field val connector))

(define (set connector val)
  (send connector set val 'user))

(define (forget connector)
  (send connector forget 'user))

(define constraint%
  (class object%
    (super-new)

    (define/public (set connector val)
      (send connector set val this))

    (define/public (forget connector)
      (send connector forget this))

    (define/public (connect connector)
      (send connector connect this))))

(define probe%
  (class constraint%
    (super-new)
    (inherit connect)

    (init-field name connector)

    (define (print val)
      (printf "~a = ~a\n" name val))

    (define/public (inform)
      (print (get connector)))

    (define/public (retract)
      (print "?"))

    (connect connector)))

(define (probe name connector)
  (make-object probe% name connector))

(define constant%
  (class constraint%
    (super-new)
    (inherit set connect)

    (init-field val connector)

    (define/public (inform)
      (error "can't redefine a constant"))

    (define/public (retract)
      (error "can't redefine a constant"))

    (connect connector)
    (set connector val)))

(define (constant val connector)
  (make-object constant% val connector))

(define adder%
  (class constraint%
    (super-new)
    (inherit set forget connect)

    (init-field a1 a2 sum)

    (define/public (inform)
      (cond ((and (set? a1) (set? a2))
             (set sum (+ (get a1) (get a2))))
            ((and (set? a1) (set? sum))
             (set a2 (- (get sum) (get a1))))
            ((and (set? a2) (set? sum))
             (set a1 (- (get sum) (get a2))))))

    (define/public (retract)
      (for ((connector (list a1 a2 sum)))
        (forget connector)))

    (for ((connector (list a1 a2 sum)))
      (connect connector))))

(define (adder a1 a2 sum)
  (make-object adder% a1 a2 sum))

(define multiplier%
  (class constraint%
    (super-new)
    (inherit set forget connect)

    (init-field m1 m2 product)

    (define/public (inform)
      (cond ((and (set? m1) (set? m2))
             (set product (* (get m1) (get m2))))
            ((and (set? m1) (set? product))
             (set m2 (/ (get product) (get m1))))
            ((and (set? m2) (set? product))
             (set m1 (/ (get product) (get m2))))))

    (define/public (retract)
      (for ((connector (list m1 m2 product)))
        (forget connector)))

    (for ((connector (list m1 m2 product)))
      (connect connector))))

(define (multiplier m1 m2 product)
  (make-object multiplier% m1 m2 product))

(define (celsius-fahrenheit-converter c f)
  (let ((u (new connector%))
        (v (new connector%))
        (w (new connector%))
        (x (new connector%))
        (y (new connector%)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)))
