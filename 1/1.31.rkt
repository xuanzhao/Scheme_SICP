#lang planet neil/sicp


(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product- term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (identity x) x)
(define (inc n) (+ n 1))
  
(product identity 1 inc 4)

(define (factorial n)
  (product identity 1 inc n))

(factorial 4)

(product identity 1 inc 10)
(factorial 10)

(define (numer-term i)
  (cond ((= i 1) 2)
        ((even? i) (+ i 2))
        (else (+ i 1))))

(define (denom-term i)
  (if (odd? i)
      (+ i 2)
      (+ i 1)))

(define (pi n)
  (* 4
     (exact->inexact
      (/ (product numer-term 1 inc n)
         (product denom-term 1 inc n)))))

(pi 1)
(pi 10)
(pi 100)
   