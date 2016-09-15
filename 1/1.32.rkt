#lang planet neil/sicp

(define (accumulate combi null term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combi (term a) result))))
  (iter a null))

(define (identity x) x)
(define (inc n) (+ n 1))
  
(define (accumulate- combi null term a next b)
  (if (> a b)
      null
      (combi (term a) (accumulate combi null term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

(sum identity 1 inc 10)
(product identity 1 inc 10)

