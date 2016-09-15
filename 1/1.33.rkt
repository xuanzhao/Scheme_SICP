#lang planet neil/sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next divisor)
  (if (= 2 divisor)
      3
      (+ divisor 2)))

(define (identity x) x)
(define (inc n) (+ n 1))
  

(define (filtered-accumulate- filter combi null term a next b)
  (if (> a b)
      null
      (if (filter a)
          (combi (term a) (filtered-accumulate filter combi null term (next a) next b))
          (filtered-accumulate filter combi null term (next a) next b))))

(define (filtered-accumulate filter combi null term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combi (term a) result))
            (iter (next a) result))))
  (iter a null))


         
(define (primes-sum a b)
  (filtered-accumulate prime?
                       +
                       0
                       identity
                       a
                       inc
                       b))

(define (coprime? i n)
  (and (< i n)
       (= 1 (gcd i n))))

(define (gcd i n)
  (if (= n 0)
      i
      (gcd n (remainder i n))))

(define (product-coprimes n)
  (filtered-accumulate (lambda (x) (coprime? x n))
                       *
                       1
                       identity
                       1
                       inc
                       n))
                       

(primes-sum 1 10)

(product-coprimes 10)




