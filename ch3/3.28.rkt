#lang sicp

(define (make-queue)
  (let* ((front-ptr '())
         (rear-ptr '())
         (empty? (lambda ()
                   (null? front-ptr)))
         (front (lambda ()
                  (if (empty?)
                      '()
                      (car front-ptr))))
         (insert! (lambda (item)
                    (let ((node (cons item '())))
                      (if (empty?)
                          (set! front-ptr node)
                          (set-cdr! rear-ptr node))
                      (set! rear-ptr node))))
         (delete! (lambda ()
                    (if (empty?)
                        (error "make-queue: delete!: empty queue")
                        (set! front-ptr (cdr front-ptr)))))
         (dispatch (lambda (m)
                     (case m
                       ('empty? empty?)
                       ('front front)
                       ('insert! insert!)
                       ('delete! delete!)
                       (else (error "queue: unknown method:" m))))))
    dispatch))

(define (empty-queue? q)
  ((q 'empty?)))

(define (front-queue q)
  ((q 'front)))

(define (insert-queue! q item)
  ((q 'insert!) item)
  q)

(define (delete-queue! q)
  ((q 'delete!))
  q)

(define (call-each l)
  (for-each (lambda (proc)
              (proc))
            l))

(define (make-wire)
  (let* ((value 0)
         (actions '())
         (set-signal! (lambda (n)
                        (if (not (= value n))
                            (begin (set! value n)
                                   (call-each actions)))))
         (add-action! (lambda (proc)
                        (set! actions
                              (cons proc actions))))
         (dispatch (lambda (m)
                     (case m
                       ('get-signal value)
                       ('set-signal! set-signal!)
                       ('add-action! add-action!)
                       (else (error "wire: unknown method:" m))))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire n)
  ((wire 'set-signal!) n))

(define (add-action! wire proc)
  ((wire 'add-action!) proc))

(define (make-agenda)
  (let* ((current-time 0)
         (segments '())
         (make-segment (lambda (time action)
                         (let ((q (make-queue)))
                           (insert-queue! q action)
                           (cons time q))))
         (segment-time car)
         (segment-queue cdr)
         (empty-agenda? (lambda ()
                          (null? segments)))
         (first-item (lambda ()
                       (if (empty-agenda?)
                           (set! current-time
                                 (segment-time
                                  (car segments)))
                           (front-queue (segment-queue
                                         (car segments))))))
         (belongs-before? (lambda (time segments)
                            (or (null? segments)
                                (< time (segment-time
                                         (car segments))))))
         (add-to-agenda! (lambda (time action)
                           (letrec ((a (lambda (segments)
                                         (if (= time (segment-time
                                                      (car segments)))
                                             (insert-queue! (segment-queue
                                                             (car segments))
                                                            action)
                                             (if (belongs-before? time
                                                                  (cdr segments))
                                                 (set-cdr! segments
                                                           (cons (make-segment time action)
                                                                 (cdr segments)))
                                                 (a (cdr segments)))))))
                             (if (belongs-before? time segments)
                                 (cons (make-segment time action)
                                       (cdr segments))
                                 (a segments)))))
         (remove-first-item! (lambda ()
                               (let ((q (segment-queue (car segments))))
                                 (delete-queue! q)
                                 (if (empty-queue? q)
                                     (set! segments (cdr segments))))))
         (dispatch (lambda (m)
                     (case m
                       ('current-time current-time)
                       ('empty-agenda? empty-agenda?)
                       ('first-item first-item)
                       ('add-to-agenda! add-to-agenda!)
                       ('remove-first-item! remove-first-item!)
                       (else (error "agenda: unknown method:" m))))))
    dispatch))

(define (current-time agenda)
  (agenda 'current-time))

(define (empty-agenda? agenda)
  ((agenda 'empty-agenda?)))

(define (first-item agenda)
  ((agenda 'first-item)))

(define (add-to-agenda! agenda time action)
  ((agenda 'add-to-agenda!) time action))

(define (remove-first-item! agenda)
  ((agenda 'remove-first-item!)))

;(define (after-delay delay action)
;  (add-to-agenda! (+ delay (current-time the-agenda))
;                  action
;                  the-agenda))
;
;(define (propagate)
;  (if (empty-agenda? the-agenda)
;      'done
;      (let ((first-item (first-agenda-item the-agenda)))
;        (first-item)
;        (remove-first-agenda-item! the-agenda)
;        (propagate))))
;
;(define (logical-not s)
;  (cond ((zero? s) 1)
;        ((= s 1) 0)
;        (else (error "logical-not: invalid signal:" s))))
;
;(define inverter-delay 2)
;
;(define (inverter input output)
;  (let* ((new-value (logical-not (get-signal input)))
;         (invert-input (lambda ()
;                         (after-delay inverter-delay
;                                      (lambda ()
;                                        (set-signal! output new-value))))))
;    (add-action! input invert-input)
;    'ok))
;
;(define (logical-and s1 s2)
;  (cond ((and (= s1 1)
;              (= s2 1)) 1)
;        ((or (and (zero? s1)
;                  (= s2 1))
;             (and (= s1 1)
;                  (zero? s2))) 0)
;        (else (error "logical-and: invalid signal:" (list s1 s2)))))
;
;(define and-gate-delay 3)
;
;(define (and-gate a1 a2 output)
;  (let* ((new-value (logical-and (get-signal a1)
;                                 (get-signal a2)))
;         (and-action-procedure (lambda ()
;                                 (after-delay and-gate-delay
;                                              (lambda ()
;                                                (set-signal! output new-value))))))
;    (add-action! a1 and-action-procedure)
;    (add-action! a2 and-action-procedure)
;    'ok))
;
;(define (logical-or s1 s2)
;  (cond ((and (zero? s1)
;              (zero? s2)) 0)
;        ((or (and (zero? s1)
;                  (= s2 1))
;             (and (= s1 1)
;                  (zero? s2))
;             (and (= s1 1)
;                  (= s2 1))) 1)
;        (else (error "logical-or: invalid signal:" (list s1 s2)))))
;
;(define or-gate-delay 5)
;
;(define (or-gate a1 a2 output)
;  (let* ((new-value (logical-or (get-signal a1)
;                                (get-signal a2)))
;         (or-action-procedure (lambda ()
;                                (after-delay or-gate-delay
;                                             (lambda ()
;                                               (set-signal! output new-value))))))
;    (add-action! a1 or-action-procedure)
;    (add-action! a2 or-action-procedure)
;    'ok))
