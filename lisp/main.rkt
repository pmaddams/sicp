#lang racket/base

(provide
 ; implicit forms
 #%app
 #%datum
 #%module-begin
 #%top
 #%top-interaction

 ; special forms
 quote
 lambda
 define
 set!
 if
 begin
 cond
 else
 and
 or
 let

 ; builtins
 boolean?
 number?
 string?
 symbol?
 procedure?
 +
 -
 *
 /
 >
 <
 =
 eq?
 not
 null?
 pair?
 cons
 car
 cdr
 list
 display
 newline
 read
 load
 error
 subst
 define-var
 get-var
 set-var

 ; reserved
 (rename-out (apply apply*)))

(define (subst vars vals env)
  (if (= (length vars) (length vals))
      (let ((frame (make-hash (map cons vars vals))))
        (cons frame env))
      (error "arity mismatch:" vars vals)))

(define (define-var var val env)
  (let ((frame (car env)))
    (hash-set! frame var val)))

(define (get-var var env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-ref frame var)
            (get-var var (cdr env))))))

(define (set-var var val env)
  (if (null? env)
      (error "undefined:" var)
      (let ((frame (car env)))
        (if (hash-has-key? frame var)
            (hash-set! frame var val)
            (set-var var val (cdr env))))))
