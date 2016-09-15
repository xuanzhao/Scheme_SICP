#lang planet neil/sicp


(define (square x)
  (* x x))

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (define (iter guess)
      (let ((new-guess (improve-guess guess)))
        (if (good-enough? new-guess)
            new-guess
            (iter new-guess))))
    (iter guess)))

(define (fixed-point f guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) 0.001))
  ((iterative-improve close-enough? f) guess))

(fixed-point cos 1)

(define (sqrt guess x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve good-enough? improve) guess))

(sqrt 1 64)
    

