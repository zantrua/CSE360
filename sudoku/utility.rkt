#lang racket

(provide square 1+ 1- tile-function)

(define (square n) (* n n))

(define (1+ x) (+ x 1))

(define (1- x) (- x 1))

(define (tile-function f width)
  (build-list width
              (λ (y) (build-list width
                                 (λ (x) (f x y))))))
