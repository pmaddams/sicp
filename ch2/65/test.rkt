#lang racket/base

(require racket/sequence
         (prefix-in set: racket/set)
         rackunit
         "main.rkt")

(test-case
 "basic tests"
 (for ((l (in-list `(()
                     (0)
                     ,(sequence->list (in-range -5 5))))))
   (let ((s (apply set l)))
     (check-true (set? s))
     (check-equal? (set->list s)
                   (normalize (set:set->list (apply set:set l)))))))

(test-case
 "random tests"
 (for ((i (in-range 5)))
   (let ((l (for/list ((i (in-range (random 5 10))))
              (random 10)))
         (n (random 10)))
     (check-equal? (set->list (set-add (list->set l) n))
                   (normalize (set:set->list (set:set-add (set:list->set l) n))))
     (check-equal? (set-member? (list->set l) n)
                   (set:set-member? (set:list->set l) n)))))

(test-case
 "set-union"
 (let ((l1 (for/list ((i (in-range (random 5 10))))
             (random 10)))
       (l2 (for/list ((i (in-range (random 5 10))))
             (random 10))))
   (check-equal? (set->list (set-union (list->set l1) (list->set '())))
                 (set->list (list->set l1)))
   (check-equal? (set->list (set-union (list->set '()) (list->set l2)))
                 (set->list (list->set l2)))
   (check-equal? (set->list (set-union (list->set l1)
                                       (list->set l2)))
                 (normalize (set:set->list (set:set-union (set:list->set l1)
                                                          (set:list->set l2)))))))

(test-case
 "set-intersect"
 (let ((l1 (for/list ((i (in-range (random 5 10))))
             (random 10)))
       (l2 (for/list ((i (in-range (random 5 10))))
             (random 10))))
   (check-equal? (set->list (set-intersect (list->set l1) (list->set '())))
                 '())
   (check-equal? (set->list (set-intersect (list->set '()) (list->set l2)))
                 '())
   (check-equal? (set->list (set-intersect (list->set l1)
                                           (list->set l2)))
                 (normalize (set:set->list (set:set-intersect (set:list->set l1)
                                                              (set:list->set l2)))))))
