#lang sicp

(#%require (only racket/base
                 make-hash
                 hash-ref
                 hash-set!))

(define (make-table)
  (let* ((table (make-hash))
         (get (lambda (key)
                (hash-ref table key #f)))
         (put (lambda (key value)
                (hash-set! table key value)))
         (dispatch (lambda (m)
                     (case m
                       ('get get)
                       ('put put)
                       (else (error "table: unknown method:" m))))))
    dispatch))

(define (get table key)
  ((table 'get) key))

(define (put table key value)
  ((table 'put) key value))

(define (make-register)
  (let* ((contents #f)
         (set-contents! (lambda (val)
                          (set! contents val)))
         (dispatch (lambda (m)
                     (case m
                       ('get-contents contents)
                       ('set-contents! set-contents!)
                       (else (error "register: unknown method:" m))))))
    dispatch))

(define (get-contents reg)
  (reg 'get-contents))

(define (set-contents! reg val)
  ((reg 'set-contents!) val))

(define (make-stack)
  (let* ((stack '())
         (push (lambda (val)
                 (set! stack
                       (cons val stack))))
         (pop (lambda ()
                (if (null? stack)
                    (error "stack: pop: empty stack")
                    (let ((top (car stack)))
                      (set! stack
                            (cdr stack))
                      top))))
         (init (lambda ()
                 (set! stack '())))
         (dispatch (lambda (m)
                     (case m
                       ('push push)
                       ('pop (pop))
                       ('init (init))
                       (else (error "stack: unknown method:" m))))))
    dispatch))

(define (push stack val)
  ((stack 'push) val))

(define (pop stack)
  (stack 'pop))

;;

;(define (make-instruction text)
;  (list text))
;
;(define instruction-text car)
;
;(define instruction-execution-proc cdr)
;
;(define set-instruction-execution-proc! set-cdr!)
;
;(define (make-label-entry label-name insts)
;  (cons label-name insts))
;
;(define (lookup-label labels label-name)
;  ())
;
;(define (extract-labels text)
;  (if ()
;      '(())
;      (let* ((result (extract-labels (cdr text)))
;             (insts (car result))
;             (labels (cdr result))
;             (next-inst (car text)))
;        (if (symbol? next-inst)
;            (cons insts
;                  (cons (make-label-entry next-inst insts)
;                        labels))
;            (cons (cons (make-instruction next-inst)
;                        insts)
;                  labels)))))
;
;(define (assemble text machine)
;  ())
;
;(define (make-machine regs ops text))

;(define (make-register name)
;  (let* ((contents '<unassigned>)
;         (dispatch (lambda (m)
;                     (case m
;                       ('get contents)
;                       ('set (lambda (value)
;                               (set! contents value)))
;                       (else (error "register: unknown method:" m))))))
;    dispatch))
;
;(define (get-contents register)
;  (register 'get))
;
;(define (set-contents! register value)
;  ((register 'set) value))
;
;(define (make-stack)
;  (let* ((s '())
;         (push (lambda (x)
;                 (set! s (cons x s))))
;         (pop (lambda ()
;                (if (null? s)
;                    (error "stack: pop: empty stack")
;                    (let ((top (car s)))
;                      (set! s (cdr s))
;                      top))))
;         (initialize (lambda ()
;                       (set! s '())
;                       'done))
;         (dispatch (lambda (m)
;                     (case m
;                       ('push push)
;                       ('pop (pop))
;                       ('initialize (initialize))
;                       (else (error "stack: unknown method:" m))))))
;    dispatch))
;
;(define (make-instruction text)
;  (cons text '()))
;
;(define instruction-text car)
;
;(define instruction-execution-proc cdr)
;
;(define set-instruction-execution-proc! set-cdr!)
;
;(define (make-new-machine)
;  (letrec ((pc (make-register 'pc))
;           (flag (make-register 'flag))
;           (stack (make-stack))
;           (the-instruction-sequence '())
;           (the-ops (list (list 'initialize-stack
;                                (lambda ()
;                                  (stack 'initialize)))))
;           (register-table (list (list 'pc pc)
;                                 (list 'flag flag)))
;           (allocate-register (lambda (name)
;                                (if (assoc name register-table)
;                                    (error "multiply defined register:" name)
;                                    (set! register-table
;                                          (cons (list name (make-register name))
;                                                register-table)))
;                                'register-allocated))
;           (lookup-register (lambda (name)
;                              (let ((val (assoc name register-table)))
;                                (if val
;                                    (cadr val)
;                                    (error "unknown register:" name)))))
;           (execute (lambda ()
;                      (let ((insts (get-contents pc)))
;                        (if (null? insts)
;                            'done
;                            (begin ((instruction-execution-proc (car insts)))
;                                   (execute))))))
;           (dispatch (lambda (m)
;                       (case m
;                         ('start
;                          (set-contents! pc the-instruction-sequence)
;                          (execute))
;                         ('install-instruction-sequence
;                          (lambda (seq)
;                            (set! the-instruction-sequence seq)))
;                         ('allocate-register
;                          allocate-register)
;                         ('get-register
;                          lookup-register)
;                         ('install-operations
;                          (lambda (ops)
;                            (set! the-ops (append the-ops ops))))
;                         ('stack
;                          stack)
;                         ('operations
;                          the-ops)
;                         (else (error "machine: unknown method:" m))))))
;    dispatch))
