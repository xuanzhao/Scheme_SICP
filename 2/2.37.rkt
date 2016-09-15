#lang planet neil/sicp


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (car-n seqs)
  (map car seqs))

(define (cdr-n seqs)
  (map cdr seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (car-n seqs))
            (accumulate-n op init (cdr-n seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define test-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(display test-matrix)
(display (dot-product (list 1 2 3) (list 5 6 7)))


(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(display (matrix-*-vector test-matrix (list 1 2 3 4)))

(define (transpose m)
  (accumulate-n cons nil m))

(display (transpose test-matrix))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (col-of-n) (matrix-*-vector m col-of-n)) cols)))

(display (matrix-*-matrix test-matrix (transpose test-matrix)))