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
  (let ((node (cons item '())))
    (if (empty-queue? q)
        (set-front-ptr! q node)
        (set-cdr! (rear-ptr q) node))
    (set-rear-ptr! q node)
    q))

(define (delete-queue! q)
  (if (empty-queue? q)
      (error "delete-queue!: empty queue:" q)
      (set-front-ptr! q (cdr (front-ptr q))))
  q)

;; Our queue representation stores a list of the items in the queue in
;; front-ptr, and considers the queue to be empty if front-ptr is the empty
;; list. The rear-ptr only exists for the purpose of adding items to the end of
;; the list without having to traverse the entire list each time, i.e. constant-
;; time instead of linear-time complexity for the insert-queue! operation. If
;; Ben wants to print a list of all items in the queue, he should traverse the
;; list at front-ptr and ignore rear-ptr altogether.

(define (print-queue q)
  (display (front-ptr q))
  (newline))

(let ((q (make-queue)))
  (insert-queue! q 'a)
  (print-queue q)
  (insert-queue! q 'b)
  (print-queue q)
  (delete-queue! q)
  (print-queue q)
  (delete-queue! q)
  (print-queue q))
;; (a)
;; (a b)
;; (b)
;; ()
