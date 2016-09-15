#lang planet neil/sicp


(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (count-pairs x)
  (if (or (not (pair? x))
          ((history 'visited?) x))
      0
      (begin
        ((history 'add) x)
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1))))

(define history

  (let ((visited-list (cons '() '())))

    (define (visited? x)
      (define (iter visited-list x)
        (cond ((null? visited-list) #f)
              ((eq? (car visited-list) x) #t)
              (else (iter (cdr visited-list) x))))
      (iter visited-list x))

    (define (add x)
      (append! visited-list (cons x nil)))

    (lambda (m)
      (cond ((eq? m 'add) add)
            ((eq? m 'visited?) visited?)
            ((eq? m 'reset) (set! visited-list (cons '() '())))
            (else (error "UNknow operation"))))))


(count-pairs (cons (cons 1 2) (cons 3 4)))
(count-pairs (list 1 2 3))
(count-pairs (let ((x (cons 1 2)))    ; 带有重复指针的序对
                     (cons x x)))

