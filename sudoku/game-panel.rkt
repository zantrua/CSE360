#lang racket

(require (file "puzzle.rkt")
         (file "game-board.rkt")
         (file "position.rkt")
         (file "tile.rkt")
         racket/gui)

(provide make-game-panel set-options load-game-options)

(define puzzle empty)
(define options empty)

(define mistakes-message empty)

(define (set-options difficulty size)
  (let-values ([(remove-frac avg-time hints) (case difficulty
                                               [(easy)   (values 1/81 1000 10)]
                                               [(medium) (values 0.4 1200  5)]
                                               [(hard)   (values 0.5 1500  2)]
                                               [(evil)   (values 0.6 2000  0)])])
    (set! puzzle (puzzle-unsolve (make-puzzle size) remove-frac))
    (set! options (list avg-time hints))
    (set! seconds 0)
    (set! mistakes 0)
    (set! hints-used 0)
    (set! game-difficulty difficulty)
    (send mistakes-message set-label "Mistakes: 0")))

(define (load-game-options difficulty time hints mistakes-inner board)
  (set-options difficulty 3)
  (set! game-difficulty difficulty)
  (set! seconds time)
  (set! hints-used hints)
  (set! mistakes mistakes-inner)
  (set! puzzle board)
  (send mistakes-message set-label (format "Mistakes: ~a" mistakes)))

(define (get-avg-time)
  (first options))

(define (get-hints-allowed)
  (second options))

(define seconds 0)
(define mistakes 0)
(define hints-used 0)
(define game-difficulty 'easy)

(define (calc-score)
  (- 8100 (* 100 mistakes) seconds))

(define (make-game-panel set-save-options set-score get-user-name master-panel handle-event)
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
           [hint-button (new button%
                             [parent game-bar-panel]
                             [label "Hint"]
                             [callback (λ (button event) (let ([hint (puzzle-get-hint puzzle)])
                                                           (display hint)
                                                           (set! puzzle (replace-puzzle-tile puzzle
                                                                                             (first hint)
                                                                                             (make-tile (second hint) #f empty #t)))
                                                           (set! hints-used (+ 1 hints-used))
                                                           (send game-canvas refresh)
                                                           (if (puzzle-solved? puzzle #t)
                                                               (begin (set-score (calc-score) (symbol->string game-difficulty) (get-user-name))
                                                                      (handle-event 'complete))
                                                               (void))))])]
           [time-msg (new message%
                           [parent game-bar-panel]
                           [label "Time: 0"]
                           [auto-resize #t])]
           [mistakes-msg (new message%
                              [parent game-bar-panel]
                              [label "Mistakes: 0"]
                              [auto-resize #t])]
           [hints-msg (new message%
                           [parent game-bar-panel]
                           [label "Hints left: 0"]
                           [auto-resize #t])]
           [sudoku-canvas% (class canvas%
                             (define/override (on-event event) 
                               (let* ([event-type (send event get-event-type)] 
                                      [mouse-x (send event get-x)] 
                                      [mouse-y (send event get-y)] 
                                      [mouse-pos (make-pos mouse-x mouse-y)]) 
                                 (if (or (eq? event-type 'left-down) 
                                         (eq? event-type 'right-down))
                                     (begin (set! puzzle (handle-click puzzle mouse-pos event-type))
                                            (send game-canvas refresh)
                                            (if (puzzle-solved? puzzle #t)
                                                (begin (set-score (calc-score) (symbol->string game-difficulty) (get-user-name))
                                                       (handle-event 'complete))
                                                (if (puzzle-solved? puzzle #f)
                                                    (void)
                                                    (begin (set! mistakes (+ 1 mistakes))
                                                           (send mistakes-msg set-label (format "Mistakes: ~a" mistakes))))))
                                     (void))))
                             (super-new))]
           [game-canvas (new sudoku-canvas%
                             [parent game-panel]
                             [paint-callback (λ (canvas dc) (draw-puzzle puzzle dc))])]
           [timer (new timer%
                       [interval 1000]
                       [notify-callback (λ () (set! seconds (+ 1 seconds))
                                              (send time-msg set-label (format "Time: ~a" seconds)))])])
    (set! mistakes-message mistakes-msg)
    game-panel))
