#lang racket

(require racket/gui)

(provide make-menu-panel)

(define (make-menu-panel master-panel handle-event)
  (let* ([panel (new vertical-panel%
                          [parent master-panel])]
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
         [logout-button (new button%
                                 [parent panel]
                                 [label "Log out"]
                                 [callback (λ (button event) (handle-event 'logout-button))])])
    panel))