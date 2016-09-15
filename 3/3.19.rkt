#lang planet neil/sicp

(define (append! x y)
  (set-cdr! (last-pair x)
            y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (cycled? l)
  (define (iter 1-st 2-st)
    (let ((x (cdr 1-st))
          (y (cddr 2-st)))
      (cond ((or (null? x) (null? y)) #f)
            ((eq? x y) #t)
            (else (iter x y)))))
  (iter l (cdr l)))



(cycled? (list 1 2 3))
(define circular-list (list 1 2 3))
(set-cdr! (last-pair circular-list) circular-list)

(cycled? circular-list)