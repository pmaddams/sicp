#lang sicp

(define front-ptr car)

(define rear-ptr cdr)

(define set-front-ptr! set-car!)

(define set-rear-ptr! set-cdr!)

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (make-queue)
  (cons '() '()))

(define (front-queue q)
  (if (empty-queue? q)
      (error "front-queue: empty queue:" q)
      (car (front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? q)
        (set-front-ptr! q new-pair)
        (set-cdr! (rear-ptr q) new-pair))
    (set-rear-ptr! q new-pair)
    q))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "delete-queue!: empty queue:" q)
      (set-front-ptr! q (cdr (front-ptr q))))
  q)

(define (print-queue q)
  (display (front-ptr q))
  (newline))

;; Our queue representation stores a list of the items in the queue in
;; front-ptr, and considers the queue to be empty if front-ptr is the empty
;; list. The rear-ptr only exists for the purpose of adding items to the end of
;; the list without having to traverse the entire list each time, i.e. constant-
;; time instead of linear-time complexity for the insert-queue! operation. If
;; Ben wants to print a list of all items in the queue, he should traverse the
;; list at front-ptr and ignore rear-ptr altogether.

(let ((q1 (make-queue)))
  (insert-queue! q1 'a)
  (print-queue q1)
  (insert-queue! q1 'b)
  (print-queue q1)
  (delete-queue! q1)
  (print-queue q1)
  (delete-queue! q1)
  (print-queue q1))
;; (a)
;; (a b)
;; (b)
;; ()
