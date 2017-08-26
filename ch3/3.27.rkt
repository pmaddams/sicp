#lang sicp

(define (make-table same-key?)
  (let* ((table (list '<table>))
         (assoc (lambda (key records)
                  (letrec ((a (lambda (records)
                                (cond ((null? records) #f)
                                      ((same-key? key (caar records)) (car records))
                                      (else (a (cdr records)))))))
                    (a records))))
         (lookup (lambda (key)
                   (let ((record (assoc key (cdr table))))
                     (if record
                         (cdr record)
                         #f))))
         (insert! (lambda (key value)
                    (let ((record (assoc key (cdr table))))
                      (if record
                          (set-cdr! record value)
                          (set-cdr! table
                                    (cons (cons key value)
                                          (cdr table)))))))
         (dispatch (lambda (m)
                     (cond ((eq? m 'lookup) lookup)
                           ((eq? m 'insert!) insert!)
                           (else (error "make-table: undefined operation:" m))))))
    dispatch))

(define (memoize f)
  (let* ((table (make-table equal?))
         (get (table 'lookup))
         (put (table 'insert!)))
    (lambda (x)
      (or (get x)
          (let ((result (f x)))
            (put x result)
            result)))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((zero? n) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (dec n))
                            (memo-fib (- n 2))))))))

(memo-fib 10)
;; 55

(memo-fib 1000)
;; 43466557686937456435688527675040625802564660517371780402481729089536555417949
;; 05189040387984007925516929592259308032263477520968962323987332247116164299644
;; 0906533187938298969649928516003704476137795166849228875

;; (memoize fib) would not work, because fib makes recursive calls to itself and
;; not memo-fib.