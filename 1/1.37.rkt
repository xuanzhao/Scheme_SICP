#lang planet neil/sicp

(define (cont-frac- n d k)
  (define (cf i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))


(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))

  (iter (- k 1)
        (/ (n k) (d k))))

(define (golden-ratio k)
  (+ 1
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))

(golden-ratio 10)






