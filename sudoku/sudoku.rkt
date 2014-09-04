#lang racket

; Utility functions

(define (square n)
  (* n n))

; A data type for partial puzzles

(define (make-puzzle (n 3))
  (define width (square n))
  
  (build-list width
              (λ (y) (build-list width
                                 (λ (x) 0)))))
