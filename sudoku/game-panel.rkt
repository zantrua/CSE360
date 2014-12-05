#lang racket

(require (file "puzzle.rkt")
         (file "game-board.rkt")
         (file "position.rkt")
         (file "tile.rkt")
         (file "utility.rkt")
         racket/gui)

(provide make-game-panel set-options load-game-options)

(define puzzle empty)
(define options empty)

(define hints-message empty)

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
    (send hints-message set-label (format "Hints left: ~a" (- (get-hints-allowed) hints-used)))))

(define (load-game-options difficulty time hints mistakes-inner board)
  (set-options difficulty 3)
  (set! game-difficulty difficulty)
  (set! seconds time)
  (set! hints-used hints)
  (set! mistakes mistakes-inner)
  (set! puzzle board)
  (send hints-message set-label (format "Hints left: ~a" (- (get-hints-allowed) hints-used))))

(define (get-avg-time)
  (first options))

(define (get-hints-allowed)
  (second options))

(define seconds 0)
(define mistakes 0)
(define hints-used 0)
(define game-difficulty 'easy)

(define (calc-score)
  (let ([width (puzzle-width puzzle)])
    ; 100 * completed_squares - 100 * hints_used - 50 * mistakes - 4 * seconds
    (- (* 100 (length (filter-map (λ (pos) (let ([tile (puzzle-ref puzzle pos)])
                                      (not (or (= (tile-get-value tile) 0)
                                               (tile-get-locked tile))))) (cartesian-product (range width) (range width)))))
       (* 100 hints-used) (* 50 mistakes) (* 4 seconds))))

(define (make-game-panel set-save-options set-score get-user-name master-panel handle-event)
  (letrec ([game-panel (new vertical-panel%
                            [parent master-panel]
                            [alignment '(center center)])]
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
                             [callback (λ (button event) (if (> (- (get-hints-allowed) hints-used) 0)
                                                             (let ([hint (puzzle-get-hint puzzle)])
                                                               (set! puzzle (replace-puzzle-tile puzzle
                                                                                                 (first hint)
                                                                                                 (make-tile (second hint) #f empty #t)))
                                                               (set! hints-used (+ 1 hints-used))
                                                               (send game-canvas refresh)
                                                               (send hints-msg set-label (format "Hints left: ~a" (- (get-hints-allowed) hints-used)))
                                                               (if (puzzle-solved? puzzle #t) ; If it's completely solved by the hint, win screen
                                                                   (begin (set-score (calc-score) (symbol->string game-difficulty) (get-user-name))
                                                                          (handle-event 'complete))
                                                                   (void)))
                                                             (void)))])]
           [score-button (new button%
                              [parent game-bar-panel]
                              [label "Score now"]
                              [callback (λ (button event)
                                          (set-score (calc-score) (symbol->string game-difficulty) (get-user-name))
                                          (handle-event 'complete))])]
           [time-msg (new message%
                           [parent game-bar-panel]
                           [label "Time: 0"]
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
                                            (send game-canvas refresh) ; Update the screen
                                            (if (puzzle-solved? puzzle #t) ; If it's totally solved, win screen
                                                (begin (set-score (calc-score) (symbol->string game-difficulty) (get-user-name))
                                                       (handle-event 'complete))
                                                (if (puzzle-solved? puzzle #f) ; If they made a mistake, count it
                                                    (void)
                                                    (set! mistakes (+ 1 mistakes)))))
                                     (void))))
                             (super-new))]
           [game-canvas (new sudoku-canvas%
                             [parent game-panel]
                             [paint-callback (λ (canvas dc) (draw-puzzle puzzle dc))])]
           [timer (new timer%
                       [interval 1000]
                       [notify-callback (λ () (set! seconds (+ 1 seconds))
                                              (send time-msg set-label (format "Time: ~a" seconds)))])])
    (set! hints-message hints-msg)
    game-panel))
