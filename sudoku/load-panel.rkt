#lang racket

(require racket/gui)

(provide make-load-panel)

(define game-list empty)

(define (make-load-panel save-file-path get-user-name master-panel handle-event)
  (let* ([panel (new vertical-panel%
                     [parent master-panel])]
         [game-list-inner (new list-box%
                               [parent panel]
                               [label "Games"]
                               [choices '()])]
         [load-button (new button%
                           [parent panel]
                           [label "Load"]
                           [callback (λ (button event)
                                       (void))])]
         [menu-button (new button%
                           [parent panel]
                           [label "Menu"]
                           [callback (λ (button event) (handle-event 'menu-button))])])
    (set! game-list game-list-inner)
    panel))