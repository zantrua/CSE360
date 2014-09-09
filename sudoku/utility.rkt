#lang racket

(provide square 1+ 1- cartesian-product remove-index)

(define (square n) (* n n))

(define (1+ x) (+ x 1))

(define (1- x) (- x 1))

(define (cartesian-product a b)
  (foldl (λ (lst out) (append out lst)) empty
         (map (λ (x) (map (λ (y) (cons x y)) b)) a)))

(define (remove-index lst idx)
  (append (take lst idx) (drop lst (add1 idx))))
