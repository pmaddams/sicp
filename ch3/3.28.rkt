#lang sicp

(define (make-queue)
  (let* ((front-ptr '())
         (rear-ptr '())
         (insert! (lambda (item)
                    (let ((new-pair (cons item '())))
                      (if (null? front-ptr)
                          (set! front-ptr new-pair)
                          (set-cdr! rear-ptr new-pair))
                      (set! rear-ptr new-pair))))
         (delete! (lambda ()
                    (if (null? front-ptr)
                        (error "make-queue: delete!: empty queue")
                        (set! front-ptr (cdr front-ptr)))))
         (front (lambda ()
                  (car front-ptr)))
         (print (lambda ()
                  (display front-ptr)
                  (newline)))
         (empty? (lambda ()
                   (null? front-ptr)))
         (dispatch (lambda (m)
                     (case m
                       ('insert! insert!)
                       ('delete! delete!)
                       ('print print)
                       ('front front)
                       ('empty? empty?)
                       (else (error "queue: unknown method:" m))))))
    dispatch))

(define (insert-queue! q item)
  ((q 'insert!) item))

(define (delete-queue! q)
  ((q 'delete!)))

(define (print-queue q)
  ((q 'print)))

(define (front-queue q)
  ((q 'front)))

(define (empty-queue? q)
  ((q 'empty?)))

(define make-time-segment cons)

(define segment-time car)

(define segment-queue cdr)

(define (make-agenda)
  '(0))

(define current-time car)

(define set-current-time! set-car!)

(define segments cdr)

(define set-segments! set-cdr!)

(define first-segment cadr)

(define rest-segments cddr)

(define empty-agenda? null?)

(define the-agenda (make-agenda))

(define (add-to-agenda! time action agenda)
  (letrec ((belongs-before? (lambda (segments)
                              (or (null? segments)
                                  (< time
                                     (segment-time (car segments))))))
           (make-new-time-segment (lambda (time action)
                                    (let ((q (make-queue)))
                                      (insert-queue! q action)
                                      (make-time-segment time q))))
           (add-to-segments! (lambda (segments)
                               (if (= time
                                      (segment-time (car segments)))
                                   (insert-queue! (segment-queue (car segments))
                                                  action)
                                   (let ((rest (cdr segments)))
                                     (if (belongs-before? rest)
                                         (set-cdr! segments
                                                   (cons (make-new-time-segment time action)
                                                         rest))
                                         (add-to-segments! rest)))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
          (set-segments! agenda
                         (cons (make-new-time-segment time action)
                               segments))
          (add-to-segments! segments)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "first-agenda-item: agenda is empty:" agenda)
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-wire)
  (let* ((signal-value 0)
         (action-procedures '())
         (set-my-signal! (lambda (new-value)
                           (letrec ((call-each (lambda (procedures)
                                                 (if (null? procedures)
                                                     'done
                                                     (begin ((car procedures))
                                                            (call-each (cdr procedures)))))))
                             (if (not (= signal-value new-value))
                                 (begin (set! signal-value new-value)
                                        (call-each action-procedures))
                                 'done))))
         (accept-action-procedure! (lambda (proc)
                                     (set! action-procedures
                                           (cons proc action-procedures))
                                     (proc)))
         (dispatch (lambda (m)
                     (case m
                       ('get-signal signal-value)
                       ('set-signal! set-my-signal!)
                       ('add-action! accept-action-procedure!)
                       (else (error "wire: unknown method:" m))))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (logical-not s)
  (cond ((zero? s) 1)
        ((= s 1) 0)
        (else (error "logical-not: invalid signal:" s))))

(define inverter-delay 2)

(define (inverter input output)
  (let* ((new-value (logical-not (get-signal input)))
         (invert-input (lambda ()
                         (after-delay inverter-delay
                                      (lambda ()
                                        (set-signal! output new-value))))))
    (add-action! input invert-input)
    'ok))

(define (logical-and s1 s2)
  (cond ((and (= s1 1)
              (= s2 1)) 1)
        ((or (and (zero? s1)
                  (= s2 1))
             (and (= s1 1)
                  (zero? s2))) 0)
        (else (error "logical-and: invalid signal:" (list s1 s2)))))

(define and-gate-delay 3)

(define (and-gate a1 a2 output)
  (let* ((new-value (logical-and (get-signal a1)
                                 (get-signal a2)))
         (and-action-procedure (lambda ()
                                 (after-delay and-gate-delay
                                              (lambda ()
                                                (set-signal! output new-value))))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok))

(define (logical-or s1 s2)
  (cond ((and (zero? s1)
              (zero? s2)) 0)
        ((or (and (zero? s1)
                  (= s2 1))
             (and (= s1 1)
                  (zero? s2))
             (and (= s1 1)
                  (= s2 1))) 1)
        (else (error "logical-or: invalid signal:" (list s1 s2)))))

(define or-gate-delay 5)

(define (or-gate a1 a2 output)
  (let* ((new-value (logical-or (get-signal a1)
                                (get-signal a2)))
         (or-action-procedure (lambda ()
                                (after-delay or-gate-delay
                                             (lambda ()
                                               (set-signal! output new-value))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))
