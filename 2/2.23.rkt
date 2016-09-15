#lang racket

(define (for-each p lst)
  (if (not (null? lst))
      (begin
        (p (car lst))
        (for-each p (cdr lst)))
      (newline)))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88)) 