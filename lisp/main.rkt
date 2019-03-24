#lang racket/base

(provide
 ; racket
 #%app
 #%datum
 #%expression
 #%module-begin
 #%provide
 #%require
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

 ; input/output
 display
 newline
 error

 (rename-out
  ; mutable pairs
  (mpair? pair?)
  (mcons cons)
  (mcar car)
  (mcdr cdr)
  (mcdr cdr)
  (set-mcar! set-car!)
  (set-mcdr! set-cdr!)

  ; reserved
  (apply apply*)))
