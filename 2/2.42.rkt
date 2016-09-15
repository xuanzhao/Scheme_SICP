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


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (empty-board nil))

(define (safe? k positions)
  (define (iter-check rest-of-queen)
    (if (null? rest-of-queen)
        #t
        (let ((row-of-current-queen (car rest-of-queen))
              (row-of-next-queen (cdar rest-of-queen)))
          (if (or (= row-of-current-queen row-of-next-queen)
                  (= row-of-new-queen (+ row-of-current-queen 1))
                  (= row-of-new-queen (- row-of-current-queen 1)))
              #f
              (iter-check (cdr rest-of-queen))))))
  (iter-check positions)
  (display k))


  




