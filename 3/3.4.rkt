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
  (let ((attempts 0))
    (lambda (password m)
      (if (eq? password account-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        ; just return complaint, but has to accept argument
        (lambda (n) 
          (set! attempts (+ attempts 1))
          (if (>= attempts 7)
            "Lizard detected. Calling cops"
            "Incorrect password"))))))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'some 'deposit) 50)
((acc 'some 'deposit) 50)
((acc 'some 'deposit) 50)
((acc 'some 'deposit) 50)
((acc 'some 'deposit) 50)
((acc 'some 'deposit) 50)
((acc 'some 'deposit) 50)
        


