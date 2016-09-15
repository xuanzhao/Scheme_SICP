#lang planet neil/sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor )))))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
      

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-square-root? base m) 0)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (nontrivial-square-root? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (square a) n))))

(define (Miller-Rabin-test n)
  (let ((times (ceiling (/ n 2))))
    (test-iter n times)))

(define (test-iter n times)
  (cond ((= times 0) #t)
        ((= (expmod (+ 1 (random (- n 1))) (- n 1) n) 1)
         (test-iter n (- times 1)))
        (else #f)))

(Miller-Rabin-test 561)
(Miller-Rabin-test 1105)
   