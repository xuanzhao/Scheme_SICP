#lang planet neil/sicp


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= (car set) x) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1))
               (x2 (car s2)))
           (cond ((equal? x1 x2) (cons x1 (union-set (cdr s1) (cdr s2))))
                 ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
                 ((> x1 x2) (cons x2 (union-set s1 (cdr s2)))))))))

(display (union-set (list 1 2 3) (list 1 3 4)))

(adjoin-set 3 (list 1 2 4))



        


