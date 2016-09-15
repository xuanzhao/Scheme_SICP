;#lang planet neil/sicp

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* x (cos y)))
          ((eq? op 'imag-part)
           (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle y))
          (else
           (error "Unknown op -- MAKE_FROM_MAG_ANG" op))))
  dispatch)

        


