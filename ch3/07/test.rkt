#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "account"
 (let ((acc (new account%
                 (password "rosebud")
                 (balance 1000000))))
   (check-equal? (send acc withdraw "password" 1000000)
                 "incorrect password")
   (check-equal? (send acc withdraw "rosebud" 1)
                 999999)
   (check-equal? (send acc add-user "rosebud" "iloveyou")
                 "added new user")
   (check-equal? (send acc withdraw "iloveyou" 1000000)
                 "insufficient funds")
   (check-equal? (send acc withdraw "iloveyou" 999999)
                 0)
   (for ((password (in-list '("123456" "qwerty" "abc123" "letmein" "654321" "zxcvbnm" "asdfgh"))))
     (check-equal? (send acc withdraw password 1000000)
                   "incorrect password"))
   (check-equal? (send acc withdraw "batman" 1000000)
                 "calling 911")))
