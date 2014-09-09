#lang racket

(require (file "utility.rkt")
         (file "position.rkt"))
(provide make-partial-puzzle partial-puzzle-width partial-puzzle-ref partial-puzzle-ref-tile partial-puzzle-ref-numbers partial-puzzle->puzzle)

(define (make-partial-puzzle width)
  (tile-function (λ (x y) (list 0 (build-list width (λ (n) (1+ n)))))))

(define (partial-puzzle-width p)
  (length p))

(define (partial-puzzle-ref p pos)
  (let-values ([(x y) (pos-get-values pos)])
    (list-ref (list-ref p y) x)))

(define (partial-puzzle-ref-tile p pos)
  (let-values ([(x y) (pos-get-values pos)])
    (first (partial-puzzle-ref p x y))))

(define (partial-puzzle-ref-numbers p pos)
  (let-values ([(x y) (pos-get-values pos)])
    (second (partial-puzzle-ref p x y))))
  
(define (partial-puzzle->puzzle p)
  (tile-function (λ (x y) (partial-puzzle-ref-tile p x y)) (partial-puzzle-width p)))