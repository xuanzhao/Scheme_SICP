#lang planet neil/sicp

(define (make-point a b)
  (cons a b))

(define (x-point x) (car x))
(define (y-point y) (cdr y))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-segment s e)
  (cons s e))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

(define (average a b)
  (/ (+ a b) 2.0))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-rectangle hor vec)
  (cons hor vec))

(define (hor-side r)
  (car r))

(define (vec-side r)
  (cdr r))

(define (length seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (if (= (x-point start) (x-point end))
         (abs (- (y-point end) (y-point start)))
         (abs (- (x-point end) (x-point start))))))

(define (perimeter-rectangle r)
  (let ((hor-side (length (hor-side r)))
        (vec-side (length (vec-side r))))
    (* 2
       (+ hor-side vec-side))))

(define (area-rectangle r)
  (* (length (hor-side r)) (length (vec-side r))))

(define l (make-segment (make-point 1 2)
                        (make-point 4 2)))

(define w (make-segment (make-point 1 2)
                        (make-point 1 4)))

(define r (make-rectangle l w))

(length (hor-side r))
(length (vec-side r))

(perimeter-rectangle r)
(area-rectangle r)

    

