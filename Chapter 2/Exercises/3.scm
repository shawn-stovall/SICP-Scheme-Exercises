#lang scheme
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg)))
                 2.0)
              (/ (+ (y-point (start-segment seg))
                    (y-point (end-segment seg)))
                 2.0)))

(define (width-rectangle rect)
  (abs (- (x-point (right-corner rect))
          (x-point (left-corner rect)))))

(define (height-rectangle rect)
  (abs (- (y-point (right-corner rect))
          (y-point (left-corner rect)))))

(define (left-corner rect)
  (car rect))

(define (right-corner rect)
  (cdr rect))

(define (make-rectangle p height width)
  (cons p
        (make-point (+ (x-point p)
                       width)
                    (+ (y-point p)
                       height))))

(define (perimeter-rect rect)
  (+ (* (width-rectangle rect) 2)
     (* (height-rectangle rect) 2)))

(define (area-rect rect)
  (* (width-rectangle rect)
     (height-rectangle rect)))