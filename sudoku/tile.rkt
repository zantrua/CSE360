#lang racket

(provide make-empty-tile make-tile tile-get-value tile-get-locked tile-get-marked)

; A tile needs to know what value is in it currently and if it's allowed to change, along with the marked numbers
(define (make-empty-tile)
  (make-tile 0))

(define (make-tile value (locked #f) (marked '()))
  (list value locked marked))

(define (tile-get-value tile)
  (first tile))

(define (tile-get-locked tile)
  (second tile))

(define (tile-get-marked tile)
  (third tile))