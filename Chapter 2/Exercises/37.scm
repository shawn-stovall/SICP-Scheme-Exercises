#lang scheme
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (firsts seqs))
            (accumulate-n op init (rests seqs)))))

(define (firsts l)
  (if (null? l)
      '()
      (cons (car (car l))
            (firsts (cdr l)))))

(define (rests l)
  (if (null? l)
      '()
      (cons (cdr (car l))
            (rests (cdr l)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))
         m)))