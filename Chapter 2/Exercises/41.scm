#lang scheme
(define (permutations s)
  (if (null? s)
      (list '())
      (flat-map (lambda (x)
                  (map (lambda (p) (cons x p))
                       (permutations (remove x s))))
                s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flat-map proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (unique-trips n)
  (flat-map (lambda (i)
              (flat-map (lambda (j)
                          (map (lambda (k) (list i j k))
                               (remove i (remove j (enumerate-interval 1 n)))))
                        (remove i (enumerate-interval 1 n))))
            (enumerate-interval 1 n)))

(define (ordered-sums n s)
  (filter (lambda (x)
            (= (+ (car x)
                  (cadr x)
                  (caddr x))
               s))
          (unique-trips n)))