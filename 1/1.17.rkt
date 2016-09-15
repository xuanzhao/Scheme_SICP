#lang planet neil/sicp

(define (double a)
  (+ a a))

(define (halve b)
  (/ b 2))

(define (multi a b)
  (cond ((= b 0) 0)
        ((even? b) (double (multi a (halve b))))
        ((odd? b) (+ a (multi a (- b 1))))))

(multi 2 4)
(multi 3 3)



