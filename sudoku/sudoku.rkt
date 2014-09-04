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
(define (puzzle-width p)
  (length p))

(define (puzzle-ref p x y)
  (list-ref (list-ref p y) x))

(define (puzzle-print p)
  (define width (puzzle-width p))
  (for ([r width])
    (display (format "~%+---+---+---+---+---+---+---+---+---+~%|"))
    (for ([c width])
      (display (format " ~A |" (puzzle-ref p r c)))))
  (display (format "~%+---+---+---+---+---+---+---+---+---+~%~%")))
