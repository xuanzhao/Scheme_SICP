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

(define (e k)
  (define (n i) 1)
  (define (d i)
    (if (= 0 (remainder (+ i 1) 3))
        (* 2 (/ (+ i 1) 3))
        1))
  (+ 2.0
     (cont-frac n d k)))

(e 10)






