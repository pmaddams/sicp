#lang racket/base

; Exercise 3.37

(provide (all-defined-out))

(require racket/class)

(define connector%
  (class object%
    (super-new)

    (field (val (void))
           (informant #f)
           (constraints '()))

    (define/public (set?) (and informant #t))

    (define/public (set new-val setter)
      (if (set?)
          (unless (= val new-val)
            (error "contradiction:" val new-val))
          (begin (set! val new-val)
                 (set! informant setter)
                 (for ((constraint (in-list constraints)))
                   (unless (or (eq? constraint setter)
                               (eq? constraint 'user))
                     (send constraint inform))))))

    (define/public (forget retractor)
      (when (eq? informant retractor)
        (begin (set! informant #f)
               (for ((constraint (in-list constraints)))
                 (unless (eq? constraint retractor)
                   (send constraint retract))))))

    (define/public (connect constraint)
      (unless (member constraint constraints)
        (set! constraints (cons constraint constraints)))
      (when (set?)
        (send constraint inform)))))

(define constraint%
  (class object%
    (super-new)

    (define/public (set conn val)
      (send conn set val this))

    (define/public (forget conn)
      (send conn forget this))

    (define/public (connect conn)
      (send conn connect this))))

(define constant%
  (class constraint%
    (super-new)
    (inherit set connect)

    (init-field conn val)

    (define/public (inform)
      (error "can't redefine a constant"))

    (define/public (retract)
      (error "can't redefine a constant"))

    (connect conn)
    (set conn val)))

(define sum%
  (class constraint%
    (super-new)
    (inherit set forget connect)

    (init-field a1 a2 s)

    (define/public (inform)
      (cond ((and (set? a1) (set? a2))
             (set s (+ (get a1) (get a2))))
            ((and (set? a1) (set? s))
             (set a2 (- (get s) (get a1))))
            ((and (set? a2) (set? s))
             (set a1 (- (get s) (get a2))))))

    (define/public (retract)
      (for ((conn (in-list (list a1 a2 s))))
        (forget conn)))

    (for ((conn (in-list (list a1 a2 s))))
      (connect conn))))

(define product%
  (class constraint%
    (super-new)
    (inherit set forget connect)

    (init-field m1 m2 p)

    (define/public (inform)
      (cond ((and (set? m1) (set? m2))
             (set p (* (get m1) (get m2))))
            ((and (set? m1) (set? p))
             (set m2 (/ (get p) (get m1))))
            ((and (set? m2) (set? p))
             (set m1 (/ (get p) (get m2))))))

    (define/public (retract)
      (for ((conn (in-list (list m1 m2 p))))
        (forget conn)))

    (for ((conn (in-list (list m1 m2 p))))
      (connect conn))))

(define (get conn)
  (get-field val conn))

(define (set conn val)
  (send conn forget 'user)
  (send conn set val 'user))

(define (set? conn)
  (send conn set?))

(define (const val)
  (let ((conn (new connector%)))
    (make-object constant% conn val)
    conn))

(define (add a1 a2)
  (let ((s (new connector%)))
    (make-object sum% a1 a2 s)
    s))

(define (sub s a1)
  (let ((a2 (new connector%)))
    (make-object sum% a1 a2 s)
    a2))

(define (mul m1 m2)
  (let ((p (new connector%)))
    (make-object product% m1 m2 p)
    p))

(define (div p m1)
  (let ((m2 (new connector%)))
    (make-object product% m1 m2 p)
    m2))

(define (celsius->fahrenheit c)
  (add (mul (div (const 9) (const 5))
            c)
       (const 32)))
