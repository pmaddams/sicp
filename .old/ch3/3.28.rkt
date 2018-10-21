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
         (run-first-item (lambda ()
                           (if (empty-agenda?)
                               (error "agenda: run-first-item: empty agenda")
                               (begin (set! current-time
                                            (segment-time
                                             (car segments)))
                                      ((front-queue (segment-queue
                                                     (car segments))))))))
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
                       ('empty-agenda? (empty-agenda?))
                       ('run-first-item (run-first-item))
                       ('add-to-agenda! add-to-agenda!)
                       ('remove-first-item! (remove-first-item!))
                       (else (error "agenda: unknown method:" m))))))
    dispatch))

(define (current-time agenda)
  (agenda 'current-time))

(define (empty-agenda? agenda)
  (agenda 'empty-agenda?))

(define (run-first-item agenda)
  (agenda 'run-first-item))

(define (add-to-agenda! agenda time action)
  ((agenda 'add-to-agenda!) time action))

(define (remove-first-item! agenda)
  (agenda 'remove-first-item!))

(define agenda (make-agenda))

(define (propagate)
  (if (not (empty-agenda? agenda))
      (begin (run-first-item agenda)
             (remove-first-item! agenda)
             (propagate))))

(define (after-delay delay action)
  (add-to-agenda! agenda
                  (+ (current-time agenda)
                     delay)
                  action))

(define (logical-not s)
  (if (zero? s)
      1
      0))

(define (inverter in out)
  (let* ((delay 2)
         (value (logical-not (get-signal in)))
         (action (lambda ()
                   (after-delay delay
                                (lambda ()
                                  (set-signal! out value))))))
    (add-action! in action)))

(define (logical-and s1 s2)
  (if (or (zero? s1)
          (zero? s2))
      0
      1))

(define (and-gate i1 i2 out)
  (let* ((delay 3)
         (value (logical-and (get-signal i1)
                             (get-signal i2)))
         (action (lambda ()
                   (after-delay delay
                                (lambda ()
                                  (set-signal! out value))))))
    (add-action! i1 action)
    (add-action! i2 action)))

(define (logical-or s1 s2)
  (if (and (zero? s1)
           (zero? s2))
      0
      1))

(define (or-gate i1 i2 out)
  (let* ((delay 5)
         (value (logical-or (get-signal i1)
                             (get-signal i2)))
         (action (lambda ()
                   (after-delay delay
                                (lambda ()
                                  (set-signal! out value))))))
    (add-action! i1 action)
    (add-action! i2 action)))
