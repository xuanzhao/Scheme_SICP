#lang planet neil/sicp

(define (equal? x y)
  (cond ((and (symbol? x) (symbol? y))
         (symbol-equal? x y))
        ((and (list? x) (list? y))
         (list-equal? x y))
        (else
         (error "wrong type input x and y --Equal?"))))

(define (symbol-equal? x y)
  (eq? x y))

(define (list-equal? x y)
  (cond ((and (null? x) (null? y))
         #t)
        ((or (null? x) (null? y))
         #f)
        ((or (eq? (car x) (car y))
             (= (car x) (car y)))
             (list-equal? (cdr x) (cdr y)))
        (else #f)))


(equal? (list 'a 'b 'c) (list 'a 'b 'c))
(equal? (list 'a 2 3) (list 'a 2 3))



