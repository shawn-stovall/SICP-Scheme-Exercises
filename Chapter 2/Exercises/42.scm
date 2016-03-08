#lang scheme
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
  (cons (cons k new-row) rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (let ((pick (get-pos k positions)))
    (define (iter positions)
      (if (null? positions)
          #t
          (and (not (= (row pick)
                       (row (car positions))))
               (not-diagonal-to? pick (car positions))
               (iter (cdr positions)))))
    (iter (cdr positions))))

(define (not-diagonal-up? pick position)
  (define (recur c r)
    (if (and (= c (column position))
             (= r (row position)))
        #f
        (if (or (= c 1)
                (< r (row position)))
            #t
            (recur (- c 1) (- r 1)))))
  (recur (- (column pick) 1) (- (row pick) 1)))

(define (not-diagonal-down? pick position)
  (define (recur c r)
    (if (and (= c (column position))
             (= r (row position)))
        #f
        (if (or (= c 1)
                (> r (row position)))
            #t
            (recur (- c 1) (+ r 1)))))
  (recur (- (column pick) 1) (+ (row pick) 1)))

(define (not-diagonal-to? pick position)
  (and (not-diagonal-up? pick position)
       (not-diagonal-down? pick position)))
        

(define (get-pos k positions)
  (if (= (column (car positions)) k)
      (car positions)
      (get-pos k (cdr positions))))

(define (row position)
  (cdr position))

(define (column position)
  (car position))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))