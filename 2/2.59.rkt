#lang planet neil/sicp


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (union-set s1 s2)
  (cond ((equal? s1 nil) s2)
        ((not (element-of-set? (car s1) s2))
         (cons (car s1) (union-set (cdr s1) s2)))
        (else (union-set (cdr s1) s2))))

(union-set (list 1 2 4) (list 4 5 6))



        


