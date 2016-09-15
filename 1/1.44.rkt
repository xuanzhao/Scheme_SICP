#lang planet neil/sicp


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

(define (smooth f)
  (define dx 0.0001)
  
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3)))

(define (n-smooth f n)
   ((repeated smooth n) f))

((n-smooth square 10) 5)




