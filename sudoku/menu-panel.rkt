#lang racket

(require racket/gui)

(provide make-menu-panel)

(define (make-menu-panel master-panel handle-event)
  (let* ([menu-panel (new vertical-panel%
                          [parent master-panel])]
         [menu-new-game-button (new button%
                                    [parent menu-panel]
                                    [label "New Game"]
                                    [callback (λ (button event) (handle-event 'new-game-button))])]
         [menu-about-button (new button%
                                 [parent menu-panel]
                                 [label "About"]
                                 [callback (λ (button event) (handle-event 'about-button))])]
         [menu-logout-button (new button%
                                 [parent menu-panel]
                                 [label "Log out"]
                                 [callback (λ (button event) (handle-event 'logout-button))])])
    menu-panel))