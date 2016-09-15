#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))



(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (* (factor k)
       (y k)))
  (define (y k)
    (f (+ a (* k h))))
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((odd? k) 4)
          (else 2)))
  (define (next k)
    (+ k 1))
           
  (if (not (even? n))
      (error "n can't be odd")
      (* (/ h 3)
         (sum term (exact->inexact 0) next n))))

(define (cube x)
  (* x x x))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)
(simpson cube 0 1 99)

    
   