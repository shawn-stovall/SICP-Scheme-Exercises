#lang scheme
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

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