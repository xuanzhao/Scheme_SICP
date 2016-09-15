#lang planet neil/sicp

(define (make-account balance account-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; action dispatcher, protected by password
  (lambda (password m)
    (if (eq? password account-password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m)))
      ; just return complaint, but has to accept argument
      (lambda (n) "Incorrect password"))))

(define (make-joint account old-pw new-pw)
  (lambda (given-pw op)
    (if (eq? new-pw given-pw)
        (account old-pw op)
        "Incorrect password")))


(define jack-acc (make-account 100 'jack-password))
(define peter-acc (make-joint jack-acc 'jack-password 'peter-password))


((peter-acc 'peter-password 'withdraw) 50)
((jack-acc 'jack-password 'withdraw) 0)


