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


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

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
  (if (fast-prime? n 3)
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
        ((fast-prime? n 3) (timed-prime-test n) (newline) (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count))))



(define (search-for-primes-new n)
    (continue-primes n 3))

(search-for-primes-new 1000)
(search-for-primes-new 10000)
(search-for-primes-new 100000)
(search-for-primes-new 1000000)