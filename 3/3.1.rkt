#lang planet neil/sicp

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))

(W1 50)
((make-withdraw 100) 10)

(define (make-accumulator init)
  (lambda (x)
    (set! init (+ init x))
    init))

(define A (make-accumulator 5))
(A 10)
(A 10)

        


