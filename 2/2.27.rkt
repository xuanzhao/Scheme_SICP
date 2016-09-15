#lang planet neil/sicp

(define (deep-reverse lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) lst)
        (else
         (reverse (list (deep-reverse (car lst))
                        (deep-reverse (cdr lst)))))))

(define (reverse lst)
  (if (null? lst)
      nil
      (append (reverse (cdr lst)) (list (car lst)))))

(define x (list (list 1 2) (list 3 4)))
x
(reverse x)
(deep-reverse x)