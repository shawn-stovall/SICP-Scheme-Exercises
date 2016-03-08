#lang scheme
(define (rand-update x)
  (let ((a 40)
        (b 3641)
        (m 729))
    (modulo (+ (* a x) b) m)))

(define random-init 1)

(define rand
  (let ((x random-init))
    (lambda (action)
      (cond ((eq? action 'generate)
             (set! x (rand-update x))
             x)
            ((eq? action 'reset)
             (lambda (y)
               (set! x y)
               x))))))