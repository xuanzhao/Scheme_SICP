#lang planet neil/sicp

(define (square x)
  (* x x))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (map op seq)
  (if (null? seq)
      nil
      (cons (op (car seq))
            (map op (cdr seq)))))

(define (filter op seq)
  (if (null? seq)
      nil
      (if (op (car seq))
          (cons (car seq) (filter op (cdr seq)))
          (filter op (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate  append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda(j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(display (unique-triples 4))

(define (triple-sum-equal? s triple)
  (= s
     (accumulate + 0 triple)))

(triple-sum-equal? 6 (list 1 2 3))
(triple-sum-equal? 6 (list 1 3 3))

(define (remove-triples-not-equal-sum s n)
  (filter (lambda (triple)
            (triple-sum-equal? s triple))
          (unique-triples n)))

(display (remove-triples-not-equal-sum 8 11))


