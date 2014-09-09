#lang racket

(require (file "sudoku.rkt")
         (file "position.rkt"))

(define (assert x)
  (if x (void) (error "Tests failed")))

(define (with-output-port fn)
  (let ([output (current-output-port)]
        [str (open-output-string)])
    (current-output-port str)
    (fn)
    (current-output-port output)
    (get-output-string str)))

(assert (= (pos-get-x (make-pos 123 321)) 123))
(assert (= (pos-get-y (make-pos 123 321)) 321))
(assert (pos=? (make-pos 0 0) (make-pos 0 0)))
(assert (not (pos=? (make-pos 0 1) (make-pos 0 0))))
(assert (pos=? (make-pos 0 0) (pos-backward (make-pos 1 0) 9)))
(assert (pos=? (make-pos 8 0) (pos-backward (make-pos 0 1) 9)))
(assert (pos=? (make-pos 2 0) (pos-forward (make-pos 1 0) 9)))
(assert (pos=? (make-pos 0 1) (pos-forward (make-pos 8 0) 9)))
(assert (string=? (with-output-port (λ () (pos-print (make-pos 1 2)))) "(1, 2)"))

(define test-board-zero
  '((0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)))

(assert (= (puzzle-width test-board-zero) 9))
(assert (= (puzzle-ref test-board-zero (make-pos 0 0)) 0))
(assert (equal? test-board-zero (make-empty-puzzle)))
(assert (not (puzzle-solved? test-board-zero #t)))

(define test-board-unsolved
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 3)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

(assert (= (puzzle-width test-board-unsolved) 9))
(assert (= (puzzle-ref test-board-unsolved (make-pos 0 0)) 5))
(assert (= (puzzle-ref test-board-unsolved (make-pos 1 0)) 3))
(assert (not (puzzle-solved? test-board-unsolved #t)))
(assert (puzzle-solved? test-board-unsolved))

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

(assert (puzzle-solved? test-board-solved #t))

(define test-board-incorrect
  '((5 3 4 6 7 8 9 2 1) ; the last two elements are flipped
    (6 7 2 1 9 5 3 4 8) ; so this board is incorrect
    (1 9 8 3 4 2 5 6 7)
    (8 5 9 7 6 1 4 2 3)
    (4 2 6 8 5 3 7 9 1)
    (7 1 3 9 2 4 8 5 6)
    (9 6 1 5 3 7 2 8 4)
    (2 8 7 4 1 9 6 3 5)
    (3 4 5 2 8 6 1 7 9)))

(assert (not (puzzle-solved? test-board-incorrect)))

(define test-board-incorrect-2
  '((5 3 4 6 7 8 9 1 1) ; the last two elements are repeated
    (6 7 2 1 9 5 3 4 8) ; so this board is incorrect
    (1 9 8 3 4 2 5 6 7)
    (8 5 9 7 6 1 4 2 3)
    (4 2 6 8 5 3 7 9 1)
    (7 1 3 9 2 4 8 5 6)
    (9 6 1 5 3 7 2 8 4)
    (2 8 7 4 1 9 6 3 5)
    (3 4 5 2 8 6 1 7 9)))

(assert (not (puzzle-solved? test-board-incorrect-2)))

(display "Tests completed")