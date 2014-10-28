#lang racket

(require (file "puzzle.rkt")
         (file "utility.rkt")
         (file "position.rkt")
         (file "rectangle.rkt")
         (file "game-board.rkt")
         racket/gui)

(define n 3)
(define width (square n))
(define frame-size (* 100 width))
(define square-size (/ frame-size width))

(define puzzle (puzzle-unsolve (make-puzzle n)))

(define sudoku-canvas% (class canvas%
                         (define/override (on-event event)
                           (let* ([event-type (send event get-event-type)]
                                  [mouse-x (send event get-x)]
                                  [mouse-y (send event get-y)]
                                  [mouse-pos (make-pos mouse-x mouse-y)])
                             (if (eq? event-type 'left-down)
                                 (set! puzzle (handle-click puzzle mouse-pos))
                                 (void))
                             (send canvas refresh)))
                         (super-new)))

(define frame
  (new frame%
       [label "Sudoku"]
       [min-width (+ (get-border-size) frame-size)]
       [min-height (+ (get-border-size) frame-size)]))

(define canvas (new sudoku-canvas%
                    [parent frame]
                    [paint-callback (λ (canvas dc) (draw-puzzle puzzle dc))]))

(send frame show #t)
