#lang planet neil/sicp

(define (make-rat n d)
  (if (< d 0)
      (cons (- n) (- d))
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g)))))

(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 2))
(print-rat (make-rat -1 -2))
        
    

