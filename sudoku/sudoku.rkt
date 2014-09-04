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
  (let* ([width (puzzle-width p)]
         [display-line (λ (last-line) (display (format "~%~a+~%~a"
                                                       (foldl string-append "" (build-list width (λ (n) "+---")))
                                                       (if last-line
                                                           ""
                                                           "|"))))])
    (for ([y width])
      (display-line #f)
      (for ([x width])
        (display (format " ~a |" (puzzle-ref p x y)))))
    (display-line #t)))

; Test cases

(define test-board-solved
  '((5 3 4 6 7 8 9 1 2)
    (6 7 2 1 9 5 3 4 8)
    (1 9 8 3 4 2 5 6 7)
    (8 5 9 7 6 1 4 2 3)
    (4 2 6 8 5 3 7 9 1)
    (7 1 3 9 2 4 8 5 6)
    (9 6 1 5 3 7 2 8 4)
    (2 8 7 4 1 9 6 3 5)
    (3 4 5 2 8 6 1 7 9)))
