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

(define (init-stack stack)
  (stack 'init))

(define (make-instruction text)
  (list text))

(define instruction-text car)

(define instruction-execution-proc cdr)

(define set-instruction-execution-proc! set-cdr!)

(define (add-label-entry labels label-name insts)
  (put labels label-name insts)
  labels)

(define (extract-labels text)
  (if (null? text)
      (list '() (make-table))
      (let* ((result (extract-labels (cdr text)))
             (insts (car result))
             (labels (cdr result))
             (next-inst (car text)))
        (if (symbol? next-inst)
            (cons insts
                  (add-label-entry labels next-inst insts))
            (cons (cons (make-instruction next-inst)
                        insts)
                  labels)))))

(define (make-machine-model)
  (let* ((pc (make-register))
         (flag (make-register))
         (stack (make-stack))
         (instruction-sequence '())
         (operations (list (list 'init-stack
                                 init-stack)))
         (registers (make-table))
         (install-instruction-sequence (lambda (seq)
                                         (set! instruction-sequence seq)))
         (install-operations (lambda (ops)
                               (set! operations
                                     (append operations ops))))
         (init-registers (lambda ()
                           (put registers 'pc pc)
                           (put registers 'flag flag)))
         (allocate-register (lambda (name)
                              (if (get registers name)
                                  (error "machine: multiply defined register:" name)
                                  (put registers name (make-register)))))
         (get-register (lambda (name)
                         (let ((reg (get registers name)))
                           (if reg
                               reg
                               (error "machine: unknown register:" name)))))
         (execute (letrec ((x (lambda ()
                                (let ((insts (get-contents pc)))
                                  (if (not (null? insts))
                                      (begin (instruction-execution-proc (car insts))
                                             (x)))))))
                    x))
         (dispatch (lambda (m)
                     (case m
                       ('install-instruction-sequence install-instruction-sequence)
                       ('install-operations install-operations)
                       ('get-register get-register)
                       ('allocate-register allocate-register)
                       ('stack stack)
                       ('operations operations)
                       ('execute execute)
                       (else (error "machine: unknown method:" m))))))
    (init-registers)
    dispatch))
