#lang planet neil/sicp

(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1))
                         (f x)))))))


(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)
(s 9)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)

        


