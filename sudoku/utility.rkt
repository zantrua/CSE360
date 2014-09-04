#lang racket

(provide square 1+ 1-)

(define (square n) (* n n))

(define (1+ x) (+ x 1))

(define (1- x) (- x 1))
