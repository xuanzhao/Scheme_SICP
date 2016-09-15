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

(define (tan-cf x k)

  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (define (d i)
    (- (* i 2) 1))

  (exact->inexact (cont-frac n d k)))

(tan 10)
(tan-cf 10 15)






