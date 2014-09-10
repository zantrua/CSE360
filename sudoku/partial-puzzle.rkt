#lang racket

(require (file "utility.rkt")
         (file "position.rkt"))
(provide make-partial-puzzle partial-puzzle-width partial-puzzle-ref partial-puzzle-ref-tile partial-puzzle-ref-numbers partial-puzzle->puzzle)

(define (make-partial-puzzle (n 3))
  (define width (square n))
  (tile-function (λ (pos) (list 0 (build-list width (λ (n) (1+ n))))) width))

(define (partial-puzzle-width p)
  (length p))

(define (partial-puzzle-ref p pos)
  (let-values ([(x y) (pos-get-values pos)])
    (list-ref (list-ref p y) x)))

(define (partial-puzzle-ref-tile p pos)
  (first (partial-puzzle-ref p pos)))

(define (partial-puzzle-ref-numbers p pos)
  (second (partial-puzzle-ref p pos)))
  
(define (partial-puzzle->puzzle p)
  (tile-function (λ (pos) (partial-puzzle-ref-tile p pos)) (partial-puzzle-width p)))