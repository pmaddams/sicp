#lang sicp

(define (accumulate op init seq)
  (letrec ((a (lambda (seq)
                (if (null? seq)
                    init
                    (op (car seq) (a (cdr seq)))))))
    (a seq)))

(define (filter pred? seq)
  (letrec ((f (lambda (seq)
                (cond ((null? seq) '())
                      ((pred? (car seq)) (cons (car seq) (f (cdr seq))))
                      (else (f (cdr seq)))))))
    (f seq)))

(define (enumerate-interval low high)
  (letrec ((e (lambda (i result)
                (if (< i low)
                    result
                    (e (dec i) (cons i result))))))
    (e high '())))

(define (flatmap p seq)
  (accumulate append '() (map p seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime? n)
  (letrec ((square (lambda (x)
                     (expt x 2)))
           (miller-rabin (lambda (n)
                           (letrec ((a (inc (random (dec n))))
                                    (a-expmod-n (lambda (exp)
                                                  (cond ((zero? exp) 1)
                                                        ((even? exp)
                                                         (let* ((x (a-expmod-n (/ exp 2)))
                                                                (y (remainder (square x) n)))
                                                           (if (and (= y 1)
                                                                    (not (= x 1))
                                                                    (not (= x (dec n))))
                                                               0
                                                               y)))
                                                        (else (remainder (* a (a-expmod-n (dec exp)))
                                                                         n))))))
                             (= 1 (a-expmod-n (dec n))))))
           (p (lambda (times)
                (cond ((zero? times) #t)
                      ((miller-rabin n) (p (dec times)))
                      (else #f)))))
    (cond ((<= n 1) #f)
          ((= n 2) #t)
          ((even? n) #f)
          (else (p 10)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (displayln x)
  (display x)
  (newline))

(for-each displayln (prime-sum-pairs 6))
;; (2 1 3)
;; (3 2 5)
;; (4 1 5)
;; (4 3 7)
;; (5 2 7)
;; (6 1 7)
;; (6 5 11)
