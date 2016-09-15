#lang planet neil/sicp

(define (estimate-integral p x1 y1 x2 y2 trials)
  (define rectangle-area
    (* (- x2 x1) (- y2 y1)))
  (define (predicate-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x y)))
  (* rectangle-area (monte-carlo trials predicate-test)))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (/ (random (* range 100)) 100))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (unit-predicate x y)
  (<= (+ (* (- x 3) (- x 3)) (* (- y 3) (- y 3))) 1))

(estimate-integral unit-predicate 1 1 5 10 10000)








