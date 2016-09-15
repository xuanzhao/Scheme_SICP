#lang planet neil/sicp

(define (fringe lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) (list lst))
        (else
         (append (fringe (car lst))
                 (fringe (cdr lst))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))