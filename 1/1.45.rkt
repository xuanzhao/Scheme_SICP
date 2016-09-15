#lang planet neil/sicp


(define (square x)
  (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated- f n)
  (if (= n 1)
      f
      (compose f
               (repeated f (- n 1)))))

(define (repeated-- f n)
  (define (iter i repeated-f)
    (if (= i 1)
        repeated-f
        (iter (- i 1) (compose f repeated-f))))
  (iter n f))

(define (repeated f n)
  (if (= n 1)
        f
        (lambda (x)
          (f ((repeated f (- n 1)) x)))))

(define (damps n)
  (ceiling (- (/ (log (+ n 1)) (log 2)) 1)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (average-dump f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (nth-root x n)
  (fixed-point ((repeated average-dump (damps n))
                (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0))

(nth-root 256 8)

(nth-root 100 100)

