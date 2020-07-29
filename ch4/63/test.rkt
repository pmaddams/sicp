#lang racket/base

(require rackunit
         "main.rkt")

(test-case
 "genesis"
 (initialize genesis)
 (check-equal? (query '(grandfather ?x Cain))
               '((grandfather Irad Cain)))
 (check-equal? (query '(father ?x Lamech))
               '((father Jabal Lamech)
                 (father Jubal Lamech)))
 (check-equal? (query '(grandfather ?x Methushael))
               '((grandfather Jabal Methushael)
                 (grandfather Jubal Methushael))))
