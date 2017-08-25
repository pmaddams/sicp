#lang sicp

(define (make-table same-key?)
  (let* ((table (list '<table>))
         (assoc (lambda (key records)
                  (letrec ((a (lambda (records)
                                (cond ((null? records) #f)
                                      ((same-key? key (caar records)) (car records))
                                      (else (a (cdr records)))))))
                    (a records))))
         (lookup (lambda (key1 key2)
                   (let ((subtable (assoc key1 (cdr table))))
                     (if subtable
                         (let ((record (assoc key2 (cdr subtable))))
                           (if record
                               (cdr record)
                               #f))
                         #f))))
         (insert! (lambda (key1 key2 value)
                    (let ((subtable (assoc key1 (cdr table))))
                      (if subtable
                          (let ((record (assoc key2 (cdr subtable))))
                            (if record
                                (set-cdr! record value)
                                (set-cdr! subtable
                                          (cons (cons key2 value)
                                                (cdr subtable)))))
                          (set-cdr! table
                                    (cons (list key1
                                                (cons key2 value))
                                          (cdr table)))))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'lookup) lookup)
                           ((eq? m 'insert!) insert!)
                           (else (error "make-table: undefined operation:" m))))))
    dispatch))

(define table (make-table equal?))

(define get (table 'lookup))

(define put (table 'insert!))

(define coercion-table (make-table equal?))

(define get-coercion (coercion-table 'lookup))

(define put-coercion (coercion-table 'insert!))

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

(define hierarchy
  '(integer rational real complex))

(define (higher-or-equal? type1 type2)
  (let ((l1 (memq type1 hierarchy))
        (l2 (memq type2 hierarchy)))
    (if (not (and l1 l2))
        (error "higher?: cannot compare types:" (list type1 type2))
        (<= (length l1)
            (length l2)))))

(define (highest-type . args)
  (let ((types (map type-tag args)))
    (letrec ((h (lambda (best rest)
                  (if (or (null? rest)
                          (not (memq #f (map (lambda (t)
                                               (higher-or-equal? best t))
                                             rest))))
                      best
                      (h (car rest) (cdr rest))))))
      (h (car types) (cdr types)))))

(define (apply-generic op . args)
  (let* ((t (highest-type (map type-tag args)))
         (coercion (lambda (x)
                     (let ((c (get-coercion (type-tag x) t)))
                       (if c
                           (c x)
                           #f))))
         (coerced-args (map coercion args)))
    (if (memq #f coerced-args)
        (error "apply-generic: no method for these types:"
                                       (list op (map type-tag args)))
        (apply (get op t) coerced-args))))

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
    (put 'exp '(scheme-number scheme-number)
         (lambda (x y)
           (tag (expt x y))))
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

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (integer->rational n)
  (make-rational n 1))

(put 'raise 'integer integer->rational)

(put-coercion 'integer 'rational integer->rational)

(define (rational->real n)
  (/ (numer n) (denom n)))

(put 'raise 'rational rational->real)

(put-coercion 'rational 'real rational->real)

(define (real->complex n)
  (make-complex-from-real-imag n 0))

(put 'raise 'real real->complex)

(put-coercion 'real 'complex real->complex)

(define (can-drop? n)
  (and (not (integer? n))
       (apply-generic 'equ? n
                      (apply-generic 'raise
                                     (apply-generic 'project n)))))

(define (drop n)
  (if (integer? n)
      n
      (let ((projection (apply-generic 'project n)))
        (if (apply-generic 'equ? n
                           (apply-generic 'raise projection))
            projection
            n))))