#lang planet neil/sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (search-for-primes from to)
  (cond ((>= from to) (newline) (display "done") (newline))
        ((even? from) (search-for-primes (+ from 1) to))
        (else (timed-prime-test from)
              (search-for-primes (+ from 2) to))))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))



(define (next-odd n)
  (if (odd? n)
      (+ 2 n)
      (+ 1 n)))

(define (continue-primes n count)
  (cond ((= count 0) (display "done"))
        ((prime? n) (timed-prime-test n) (newline) (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count))))



(define (search-for-primes-new n)
    (continue-primes n 3))

(search-for-primes-new 1000)
(search-for-primes-new 10000)
(search-for-primes-new 100000)
(search-for-primes-new 1000000)