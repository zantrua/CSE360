#lang racket

(require (file "utility.rkt")
         (file "position.rkt")
         (file "tile.rkt"))

(provide make-partial-puzzle
         make-partial-puzzle-tile
         partial-puzzle-width
         partial-puzzle-ref
         partial-puzzle-ref-tile
         partial-puzzle-ref-numbers
         partial-puzzle->puzzle)

(define (make-partial-puzzle-tile (n 3) (tile 0) (numbers (void)))
    (if (void? numbers)
        (let ([width (square n)])
          (list tile (build-list width (λ (n) (1+ n)))))
        (list tile numbers)))

(define (make-partial-puzzle (n 3))
  (let ([width (square n)])
    (tile-function (λ (pos) (make-partial-puzzle-tile n)) width)))

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
  (tile-function (λ (pos) (make-tile (partial-puzzle-ref-tile p pos) #t)) (partial-puzzle-width p)))