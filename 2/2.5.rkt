#lang planet neil/sicp


(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car x)
  (if (= 0 (remainder x 2))
      (+ 1 (car (/ x 2)))
      0))

(define (cdr x)
  (if (= 0 (remainder x 3))
      (+ 1 (cdr (/ x 3)))
      0))

(define x (cons 3 2))
(car x)
(cdr x)

