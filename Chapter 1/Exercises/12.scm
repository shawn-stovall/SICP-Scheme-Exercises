#lang scheme
(define (pascal row column)
  (if (or (= column 1)
          (= row column))
      1
      (+ (pascal (- row 1) (- column 1))
         (pascal (- row 1) column))))