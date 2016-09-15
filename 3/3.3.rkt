#lang planet neil/sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw op)
    (if (eq? pw password)
        (cond ((eq? op 'withdraw) withdraw)
              ((eq? op 'deposit) deposit)
              (else (error "unknown request -- MAKE-ACCOUNT" op)))
        "Incorrect password"))

  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'some 'deposit) 50)
        


