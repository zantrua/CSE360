#lang racket

(require (file "puzzle.rkt")
         (file "game-board.rkt")
         (file "position.rkt")
         racket/gui)

(provide make-game-panel set-options)

(define puzzle '())
(define options '())

(define (set-options difficulty size)
  (let-values ([(remove-frac avg-time hints) (case difficulty
                                               [(easy)   (values 0.5 1000 10)]
                                               [(medium) (values 0.6 1200  5)]
                                               [(hard)   (values 0.7 1500  2)]
                                               [(evil)   (values 0.8 2000  0)])])
    (set! puzzle (puzzle-unsolve (make-puzzle size) remove-frac))
    (set! options (list avg-time hints))))

(define (get-avg-time)
  (first options))

(define (get-hints-allowed)
  (second options))

(define seconds 0)
(define correct-cells 0)
(define hints-used 0)

(define (make-game-panel master-panel handle-event)
  (letrec ([game-panel (new vertical-panel% [parent master-panel])]
           [game-bar-panel (new horizontal-panel%
                                [parent game-panel]
                                [stretchable-height #f])]
           [game-save-button (new button%
                                  [parent game-bar-panel]
                                  [label "Save"]
                                  [callback (λ (button event) (handle-event 'game-save-button))])]
           [game-menu-button (new button%
                                  [parent game-bar-panel]
                                  [label "Menu"]
                                  [callback (λ (button event) (handle-event 'game-menu-button))])]
           [score-msg (new message%
                           [parent game-bar-panel]
                           [label "Score: 0"])]
           [sudoku-canvas% (class canvas% 
                             (define/override (on-event event) 
                               (let* ([event-type (send event get-event-type)] 
                                      [mouse-x (send event get-x)] 
                                      [mouse-y (send event get-y)] 
                                      [mouse-pos (make-pos mouse-x mouse-y)]) 
                                 (if (or (eq? event-type 'left-down) 
                                         (eq? event-type 'right-down))
                                     (begin (set! puzzle (handle-click puzzle mouse-pos event-type))
                                            (set! correct-cells (puzzle-count-correct puzzle)))
                                     (void)) 
                                 (send game-canvas refresh))) 
                             (super-new))]
           [game-canvas (new sudoku-canvas%
                             [parent game-panel]
                             [paint-callback (λ (canvas dc) (draw-puzzle puzzle dc))])]
           [calc-score (λ () (* (/ (get-avg-time) seconds)
                                (- (* correct-cells 100)
                                   (* hints-used 50))))]
           [timer (new timer%
                       [interval 1000]
                       [notify-callback (λ () (set! seconds (+ 1 seconds))
                                              (send score-msg set-label (format "Score: ~a" (calc-score))))])])
    game-panel))
