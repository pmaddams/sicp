#lang racket/base

(require racket/class
         rackunit
         "main.rkt")

(test-case
 "account"
 (let ((acc (new account%
                 (password "rosebud")
                 (balance 1000000))))
   (check-equal? (send acc withdraw "hafdksj" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "rosebud" 1)
                 999999)
   (check-equal? (send acc add-user "rosebud" "iloveyou")
                 "added user")
   (check-equal? (send acc withdraw "iloveyou" 1000000)
                 "insufficient funds")
   (check-equal? (send acc withdraw "iloveyou" 999999)
                 0)
   (check-equal? (send acc withdraw "asdfgh" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "123456" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "abc1234" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "hjfudc" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "hkliunf" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "ohewtrn" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "kcxghf" 1000000)
                 "invalid password")
   (check-equal? (send acc withdraw "batman" 1000000)
                 "dialing 911")))
