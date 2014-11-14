#lang racket

(require racket/gui)

(provide make-options-panel)

(define (make-options-panel master-panel handle-event)
  (let* ([difficulty 'easy]
         [panel (new vertical-panel%
                     [parent master-panel])]
         [difficulty-msg (new message%
                              [parent panel]
                              [label "Difficulty Level"])]
         [easy-button (new button%
                           [parent panel]
                           [label "Easy"]
                           [callback (λ (button event) (set! difficulty 'easy))])]
         [medium-button (new button%
                             [parent panel]
                             [label "Medium"]
                             [callback (λ (button event) (set! difficulty 'easy))])]
         [hard-button (new button%
                           [parent panel]
                           [label "Hard"]
                           [callback (λ (button event) (set! difficulty 'easy))])]
         [evil-button (new button%
                           [parent panel]
                           [label "Evil"]
                           [callback (λ (button event) (set! difficulty 'easy))])]
         [start-game-button (new button%
                                 [parent panel]
                                 [label "Start Game"]
                                 [callback (λ (button event) (handle-event 'start-game))])])
    panel))