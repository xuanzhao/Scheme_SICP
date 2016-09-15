#lang planet neil/sicp

(define (reverse- l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l)
              (cons (car l) result))))
  (iter l '()))

(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l)) (list (car l)))))

(reverse (list 1 4 9 16 25))

