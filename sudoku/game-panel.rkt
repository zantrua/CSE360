#lang racket

(require (file "puzzle.rkt")
         (file "game-board.rkt")
         (file "position.rkt")
         racket/gui)

(provide make-game-panel set-options load-game)

(define puzzle '())
(define options '())

(define (set-options difficulty size)
  (let-values ([(remove-frac avg-time hints) (case difficulty
                                               [(easy)   (values 0.1 1000 10)]
                                               [(medium) (values 0.6 1200  5)]
                                               [(hard)   (values 0.7 1500  2)]
                                               [(evil)   (values 0.8 2000  0)])])
    (set! puzzle (puzzle-unsolve (make-puzzle size) remove-frac))
    (set! options (list avg-time hints))
    (set! seconds 0)
    (set! mistakes 0)
    (set! hints-used 0)
    (set! game-difficulty difficulty)))

(define (load-game game)
  (void))

(define (get-avg-time)
  (first options))

(define (get-hints-allowed)
  (second options))

(define seconds 0)
(define mistakes 0)
(define hints-used 0)
(define game-difficulty 'easy)

(define (make-game-panel set-save-options master-panel handle-event)
  (letrec ([game-panel (new vertical-panel% [parent master-panel])]
           [game-bar-panel (new horizontal-panel%
                                [parent game-panel]
                                [stretchable-height #f])]
           [game-save-button (new button%
                                  [parent game-bar-panel]
                                  [label "Save"]
                                  [callback (λ (button event)
                                              (set-save-options (list game-difficulty seconds hints-used mistakes puzzle))
                                              (handle-event 'game-save-button))])]
           [game-menu-button (new button%
                                  [parent game-bar-panel]
                                  [label "Menu"]
                                  [callback (λ (button event) (handle-event 'game-menu-button))])]
           [time-msg (new message%
                           [parent game-bar-panel]
                           [label "Time: 0"]
                           [auto-resize #t])]
           [sudoku-canvas% (class canvas%
                             (define/override (on-event event) 
                               (let* ([event-type (send event get-event-type)] 
                                      [mouse-x (send event get-x)] 
                                      [mouse-y (send event get-y)] 
                                      [mouse-pos (make-pos mouse-x mouse-y)]) 
                                 (if (or (eq? event-type 'left-down) 
                                         (eq? event-type 'right-down))
                                     (set! puzzle (handle-click puzzle mouse-pos event-type))
                                     (void))
                                 (send game-canvas refresh)
                                 (if (puzzle-solved? puzzle #t)
                                     (handle-event 'complete)
                                     (void))))
                             (super-new))]
           [game-canvas (new sudoku-canvas%
                             [parent game-panel]
                             [paint-callback (λ (canvas dc) (draw-puzzle puzzle dc))])]
           [timer (new timer%
                       [interval 1000]
                       [notify-callback (λ () (set! seconds (+ 1 seconds))
                                              (send time-msg set-label (format "Time: ~a" seconds)))])])
    game-panel))
