#lang planet neil/sicp



(define (cons x y)
  (lambda (m)
    (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 3 4))
(cdr (cons 3 4))

(car (lambda (m) (m 3 4)))
((lambda (m) (m 3 4)) (lambda (p q) p))
((lambda (p q) p) 3 4)
3


