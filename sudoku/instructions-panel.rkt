#lang racket

(require racket/gui)

(provide make-instructions-panel)

(define (make-instructions-panel master-panel handle-event)
  (let* ([panel (new vertical-panel%
                           [parent master-panel]
                           [alignment '(left top)])]
         [button (new button%
                            [parent panel]
                            [label "Menu"]
                            [callback (λ (button event) (handle-event 'menu-button))])]
         [msg (new message%
                         [parent panel]
                         [label "Click the buttons and stuff"])])
    panel))
