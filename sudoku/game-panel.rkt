#lang racket

(require (file "puzzle.rkt")
         (file "game-board.rkt")
         (file "position.rkt")
         racket/gui)

(provide make-game-panel)

(define n 3)
(define puzzle (puzzle-unsolve (make-puzzle n)))

(define (make-game-panel master-panel handle-event)
  (let ([game-panel (new vertical-panel% [parent master-panel])])
    (define game-bar-panel
      (new horizontal-panel%
           [parent game-panel]
           [stretchable-height #f]))
    
    (define game-save-button
      (new button%
           [parent game-bar-panel]
           [label "Save"]
           [callback (λ (button event) (handle-event 'game-save-button))]))
    
    (define game-menu-button
      (new button%
           [parent game-bar-panel]
           [label "Menu"]
           [callback (λ (button event) (handle-event 'game-menu-button))]))
    
    (define sudoku-canvas% (class canvas% 
                             (define/override (on-event event) 
                               (let* ([event-type (send event get-event-type)] 
                                      [mouse-x (send event get-x)] 
                                      [mouse-y (send event get-y)] 
                                      [mouse-pos (make-pos mouse-x mouse-y)]) 
                                 (if (or (eq? event-type 'left-down) 
                                         (eq? event-type 'right-down))
                                     (set! puzzle (handle-click puzzle mouse-pos event-type)) 
                                     (void)) 
                                 (send game-canvas refresh))) 
                             (super-new)))
    
    (define game-canvas (new sudoku-canvas%
                             [parent game-panel]
                             [paint-callback (λ (canvas dc) (draw-puzzle puzzle dc))]))
    
    game-panel))
