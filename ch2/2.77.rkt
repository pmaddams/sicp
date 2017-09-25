#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-has-key?
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (key1 key2)
                (hash-ref table
                          (list key1 key2)
                          #f)))
         (put (lambda (key1 key2 value)
                (hash-set! table
                           (list key1 key2)
                           value)))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define table (make-table))

(define get (table 'get))

(define put (table 'put))

(define attach cons)

(define (type obj)
  (if (pair? obj)
      (car obj)
      (error "type: invalid object:" obj)))

(define (value obj)
  (if (pair? obj)
      (cdr obj)
      (error "value: invalid object:" obj)))

(define (apply-generic op . args)
  (let* ((types (map type args))
         (proc (get op types)))
    (if proc
        (apply proc (map value args))
        (error "apply-generic: no method for these types:" (list op types)))))

(define (displayln x)
  (display x)
  (newline))

(define (install-scheme-number-package)
  (let* ((tag (lambda (x)
                (attach 'scheme-number x))))
    (display "installing scheme-number package...")
    (put 'make 'scheme-number
         (lambda (x)
           (tag x)))
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
    (displayln "done.")))

(install-scheme-number-package)
;; installing scheme-number package...done.

(define make-scheme-number
  (get 'make 'scheme-number))

(define (install-rational-package)
  (let* ((numer car)
         (denom cdr)
         (make (lambda (n d)
                 (let ((g (gcd n d)))
                   (cons (/ n g)
                         (/ d g)))))
         (add (lambda (x y)
                (make (+ (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y)))))
         (sub (lambda (x y)
                (make (- (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y)))))
         (mul (lambda (x y)
                (make (* (numer x) (numer y))
                      (* (denom x) (denom y)))))
         (div (lambda (x y)
                (make (* (numer x) (denom y))
                      (* (denom x) (numer y)))))
         (tag (lambda (x)
                (attach 'rational x))))
    (display "installing rational package...")
    (put 'make 'rational
         (lambda (n d)
           (tag (make n d))))
    (put 'add '(rational rational)
         (lambda (x y)
           (tag (add x y))))
    (put 'sub '(rational rational)
         (lambda (x y)
           (tag (sub x y))))
    (put 'mul '(rational rational)
         (lambda (x y)
           (tag (mul x y))))
    (put 'div '(rational rational)
         (lambda (x y)
           (tag (div x y))))
    (displayln "done.")))

(install-rational-package)
;; installing rational package...done.

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
         (tag (lambda (x)
                (attach 'rectangular x))))
    (display "installing rectangular package...")
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
    (displayln "done.")))

(install-rectangular-package)
;; installing rectangular package...done.

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
         (tag (lambda (x)
                (attach 'polar x))))
    (display "installing polar package...")
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-mag-ang 'polar
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'make-from-real-imag 'polar
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (displayln "done.")))

(install-polar-package)
;; installing polar package...done.

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
         (add (lambda (z1 z2)
                (make-from-real-imag (+ (real-part z1) (real-part z2))
                                     (+ (imag-part z1) (imag-part z2)))))
         (sub (lambda (z1 z2)
                (make-from-real-imag (- (real-part z1) (real-part z2))
                                     (- (imag-part z1) (imag-part z2)))))
         (mul (lambda (z1 z2)
                (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                   (+ (angle z1) (angle z2)))))
         (div (lambda (z1 z2)
                (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                   (- (angle z1) (angle z2)))))
         (tag (lambda (z)
                (attach 'complex z))))
    (display "installing complex package...")
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'make-from-real-imag 'complex
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'make-from-mag-ang 'complex
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
    (put 'add '(complex complex)
         (lambda (z1 z2)
           (tag (add z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2)
           (tag (sub z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2)
           (tag (mul z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2)
           (tag (div z1 z2))))
    (displayln "done.")))

(install-complex-package)
;; installing complex package...done.

(define make-complex-from-real-imag
  (get 'make-from-real-imag 'complex))

(define make-complex-from-mag-ang
  (get 'make-from-mag-ang 'complex))

(let ((magnitude (lambda (z)
                   (apply-generic 'magnitude z)))
      (z (make-complex-from-real-imag 3 4)))
  (magnitude z))
;; 5

;; To evaluate (magnitude z), apply-generic is invoked twice. The first time, we
;; have to retrieve the definition of magnitude from the complex package, and
;; then again to get its definition from the rectangular package.
