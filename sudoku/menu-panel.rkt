#lang racket

(require racket/gui)

(provide make-menu-panel set-user-greeting)

(define greeting-message empty)

(define (set-user-greeting name)
  (send greeting-message set-label (format "Hello, ~a" name)))

(define (make-menu-panel get-user-name master-panel handle-event)
  (let* ([panel (new vertical-panel%
                          [parent master-panel])]
         [message (new message%
                       [parent panel]
                       [label "Hello, user"]
                       [auto-resize #t])]
         [new-game-button (new button%
                                    [parent panel]
                                    [label "New Game"]
                                    [callback (λ (button event) (handle-event 'new-game-button))])]
         [load-button (new button%
                                    [parent panel]
                                    [label "Load"]
                                    [callback (λ (button event) (handle-event 'load-button))])]
         [about-button (new button%
                                 [parent panel]
                                 [label "About"]
                                 [callback (λ (button event) (handle-event 'about-button))])]
         [instructions-button (new button%
                                   [parent panel]
                                   [label "Instructions"]
                                   [callback (λ (button event) (handle-event 'instructions-button))])]
         [scores-button (new button%
                             [parent panel]
                             [label "High Scores"]
                             [callback (λ (button event) (handle-event 'scores-button))])]
         [logout-button (new button%
                                 [parent panel]
                                 [label "Log out"]
                                 [callback (λ (button event) (handle-event 'logout-button))])])
    (set! greeting-message message)
    panel))