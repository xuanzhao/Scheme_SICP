#lang planet neil/sicp


(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter old-guess x)
  (let ((new-guess (improve old-guess x)))
    (if (good-enough? old-guess new-guess)
        old-guess
        (sqrt-iter new-guess x))))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old new)
  (> 0.01
     (/ (abs (- new old)) old)))

(sqrt 0.00009)
(sqrt 90000000000000000000000000000000000000000000000)