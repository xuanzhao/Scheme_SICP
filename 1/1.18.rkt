#lang planet neil/sicp

(define (multi a b)
  (multi-iter a b 0))

(define (double a)
  (+ a a))
(define (halve b)
  (/ b 2))

(define (multi-iter a b product)
  (cond ((= b 0) product)
        ((even? b) (multi-iter (double a) (halve b) product))
        ((odd? b) (multi-iter a (- b 1) (+ a product)))))

(multi 2 2)
(multi 3 3)
(multi 3 4)


