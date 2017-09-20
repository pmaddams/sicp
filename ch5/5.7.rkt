#lang sicp

(define (make-register name)
  (let* ((contents '<unassigned>)
         (dispatch (lambda (m)
                     (case m
                       ('get contents)
                       ('set (lambda (value)
                               (set! contents value)))
                       (else (error "register: unknown method:" m))))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let* ((s '())
         (push (lambda (x)
                 (set! s (cons x s))))
         (pop (lambda ()
                (if (null? s)
                    (error "stack: pop: empty stack")
                    (let ((top (car s)))
                      (set! s (cdr s))
                      top))))
         (initialize (lambda ()
                       (set! s '())
                       'done))
         (dispatch (lambda (m)
                     (case m
                       ('push push)
                       ('pop (pop))
                       ('initialize (initialize))
                       (else (error "stack: unknown method:" m))))))
    dispatch))

(define (make-instruction text)
  (cons text '()))

(define instruction-text car)

(define instruction-execution-proc cdr)

(define set-instruction-execution-proc! set-cdr!)

(define (make-new-machine)
  (letrec ((pc (make-register 'pc))
           (flag (make-register 'flag))
           (stack (make-stack))
           (the-instruction-sequence '())
           (the-ops (list (list 'initialize-stack
                                (lambda ()
                                  (stack 'initialize)))))
           (register-table (list (list 'pc pc)
                                 (list 'flag flag)))
           (allocate-register (lambda (name)
                                (if (assoc name register-table)
                                    (error "multiply defined register:" name)
                                    (set! register-table
                                          (cons (list name (make-register name))
                                                register-table)))
                                'register-allocated))
           (lookup-register (lambda (name)
                              (let ((val (assoc name register-table)))
                                (if val
                                    (cadr val)
                                    (error "unknown register:" name)))))
           (execute (lambda ()
                      (let ((insts (get-contents pc)))
                        (if (null? insts)
                            'done
                            (begin ((instruction-execution-proc (car insts)))
                                   (execute))))))
           (dispatch (lambda (m)
                       (case m
                         ('start
                          (set-contents! pc the-instruction-sequence)
                          (execute))
                         ('install-instruction-sequence
                          (lambda (seq)
                            (set! the-instruction-sequence seq)))
                         ('allocate-register
                          allocate-register)
                         ('get-register
                          lookup-register)
                         ('install-operations
                          (lambda (ops)
                            (set! the-ops (append the-ops ops))))
                         ('stack
                          stack)
                         ('operations
                          the-ops)
                         (else (error "machine: unknown method:" m))))))
    dispatch))
