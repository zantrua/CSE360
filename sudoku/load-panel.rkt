#lang racket

(require racket/gui)

(provide make-load-panel)

(define (make-load-panel save-file-path get-user-name master-panel handle-event)
  (let* ([panel (new vertical-panel%
                          [parent master-panel])]
         [game-list (new list-box%
                         [parent panel]
                         [label "Games"]
                         [choices '()])])
    panel))