#lang racket

(require racket/gui)

(provide make-about-panel)

(define (make-about-panel master-panel handle-event)
  (let* ([about-panel (new vertical-panel%
                           [parent master-panel]
                           [alignment '(left top)])]
         [about-button (new button%
                            [parent about-panel]
                            [label "Menu"]
                            [callback (λ (button event) (handle-event 'menu-button))])]
         [about-msg (new message%
                         [parent about-panel]
                         [label "This is the about screen. Look at how pretty it is."])])
    about-panel))
