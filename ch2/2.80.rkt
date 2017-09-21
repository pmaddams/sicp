#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-has-key?
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (k1 k2)
                (hash-ref table
                          (list k1 k2)
                          #f)))
         (put (lambda (k1 k2 v)
                (hash-set! table
                           (list k1 k2)
                           v)))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table))

(define get (table 'get))

(define put (table 'put))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (error "type-tag: bad tagged datum:" datum)))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "contents: bad tagged datum:" datum))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "apply-generic: no method for these types:" (list op type-tags)))))

(define (displayln x)
  (display x)
  (newline))

(define (install-scheme-number-package)
  (let* ((tag (lambda (x)
                (attach-tag 'scheme-number x))))
    (display "install-scheme-number-package...")
    (put 'add '(scheme-number scheme-number)
         (lambda (x y)
           (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y)
           (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y)
           (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y)
           (tag (/ x y))))
    (put 'equ? '(scheme-number scheme-number) =)
    (put '=zero? '(scheme-number) zero?)
    (put 'make 'scheme-number
         (lambda (x)
           (tag x)))
    (displayln "done.")))

(install-scheme-number-package)
;; install-scheme-number-package...done.

(define make-scheme-number
  (get 'make 'scheme-number))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

(define (install-rational-package)
  (let* ((numer car)
         (denom cdr)
         (make-rat (lambda (n d)
                     (let ((g (gcd n d)))
                       (cons (/ n g)
                             (/ d g)))))
         (add-rat (lambda (x y)
                    (make-rat (+ (* (numer x) (denom y))
                                 (* (numer y) (denom x)))
                              (* (denom x) (denom y)))))
         (sub-rat (lambda (x y)
                    (make-rat (- (* (numer x) (denom y))
                                 (* (numer y) (denom x)))
                              (* (denom x) (denom y)))))
         (mul-rat (lambda (x y)
                    (make-rat (* (numer x) (numer y))
                              (* (denom x) (denom y)))))
         (div-rat (lambda (x y)
                    (make-rat (* (numer x) (denom y))
                              (* (denom x) (numer y)))))
         (equ? (lambda (x y)
                 (let* ((x (make-rat (numer x) (denom y)))
                        (y (make-rat (numer y) (denom y))))
                   (and (= (numer x) (numer y))
                        (= (denom x) (denom y))))))
         (=zero? (lambda (x)
                   (zero? (numer x))))
         (tag (lambda (x)
                (attach-tag 'rational x))))
    (display "install-rational-package...")
    (put 'add '(rational rational)
         (lambda (x y)
           (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y)
           (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y)
           (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y)
           (tag (div-rat x y))))
    (put 'equ? '(rational rational) equ?)
    (put '=zero? '(rational) =zero?)
    (put 'make 'rational
         (lambda (n d)
           (tag (make-rat n d))))
    (displayln "done.")))

(install-rational-package)
;; install-rational-package...done.

(define make-rational
  (get 'make 'rational))

(define (square x)
  (expt x 2))

(define (install-rectangular-package)
  (let* ((real-part car)
         (imag-part cdr)
         (make-from-real-imag cons)
         (magnitude (lambda (z)
                      (sqrt (+ (square (real-part z))
                               (square (imag-part z))))))
         (angle (lambda (z)
                  (atan (imag-part z)
                        (real-part z))))
         (make-from-mag-ang (lambda (r a)
                              (cons (* r (cos a))
                                    (* r (sin a)))))
         (equ? (lambda (z1 z2)
                 (and (= (real-part z1) (real-part z2))
                      (= (imag-part z1) (imag-part z2)))))
         (=zero? (lambda (z)
                   (and (zero? (real-part z))
                        (zero? (imag-part z)))))
         (tag (lambda (x)
                (attach-tag 'rectangular x))))
    (display "install-rectangular-package...")
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'equ? '(rectangular rectangular) equ?)
    (put '=zero? '(rectangular) =zero?)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
    (displayln "done.")))

(define (install-polar-package)
  (let* ((magnitude car)
         (angle cdr)
         (make-from-mag-ang cons)
         (real-part (lambda (z)
                      (* (magnitude z)
                         (cos (angle z)))))
         (imag-part (lambda (z)
                      (* (magnitude z)
                         (sin (angle z)))))
         (make-from-real-imag (lambda (x y)
                                (cons (sqrt (+ (square x)
                                               (square y)))
                                      (atan y x))))
         (equ? (lambda (z1 z2)
                 (and (= (magnitude z1) (magnitude z2))
                      (= (angle z1) (angle z2)))))
         (=zero? (lambda (z)
                   (zero? (magnitude z))))
         (tag (lambda (x)
                (attach-tag 'polar x))))
    (display "install-polar-package...")
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'equ? '(polar polar) equ?)
    (put '=zero? '(polar) =zero?)
    (put 'make-from-real-imag 'polar
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
    (displayln "done.")))

(define (install-complex-package)
  (let* ((real-part (lambda (z)
                      (apply-generic 'real-part z)))
         (imag-part (lambda (z)
                      (apply-generic 'imag-part z)))
         (make-from-real-imag
          (get 'make-from-real-imag 'rectangular))
         (magnitude (lambda (z)
                      (apply-generic 'magnitude z)))
         (angle (lambda (z)
                  (apply-generic 'angle z)))
         (make-from-mag-ang
          (get 'make-from-mag-ang 'polar))
         (add-complex (lambda (z1 z2)
                        (make-from-real-imag (+ (real-part z1) (real-part z2))
                                             (+ (imag-part z1) (imag-part z2)))))
         (sub-complex (lambda (z1 z2)
                        (make-from-real-imag (- (real-part z1) (real-part z2))
                                             (- (imag-part z1) (imag-part z2)))))
         (mul-complex (lambda (z1 z2)
                        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                           (+ (angle z1) (angle z2)))))
         (div-complex (lambda (z1 z2)
                        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                           (- (angle z1) (angle z2)))))
         (equ? (lambda (z1 z2)
                   (apply-generic 'equ? z1 z2)))
         (=zero? (lambda (z)
                   (apply-generic '=zero? z)))
         (tag (lambda (z)
                (attach-tag 'complex z))))
    (display "install-complex-package...")
    (put 'add '(complex complex)
         (lambda (z1 z2)
           (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2)
           (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2)
           (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2)
           (tag (div-complex z1 z2))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'equ? '(complex complex) equ?)
    (put '=zero? '(complex) =zero?)
    (put 'make-from-real-imag 'complex
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
    (displayln "done.")))

(install-rectangular-package)
;; install-rectangular-package...done.

(install-polar-package)
;; install-polar-package...done.

(install-complex-package)
;; install-complex-package...done.

(define make-complex-from-real-imag
  (get 'make-from-real-imag 'complex))

(define make-complex-from-mag-ang
  (get 'make-from-mag-ang 'complex))

(apply-generic '=zero? 0)
;; #t

(apply-generic '=zero? 1)
;; #f

(apply-generic '=zero? (make-rational 0 1))
;; #t

(apply-generic '=zero? (make-rational 1 0))
;; #f

(apply-generic '=zero? (make-complex-from-mag-ang 0 1))
;; #t

(apply-generic '=zero? (make-complex-from-mag-ang 1 0))
;; #f

(apply-generic '=zero? (make-complex-from-real-imag 0 0))
;; #t

(apply-generic '=zero? (make-complex-from-real-imag 0 1))
;; #f
