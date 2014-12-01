#lang racket

(require racket/gui)

(provide make-options-panel)

(define (make-options-panel set-options master-panel handle-event)
  (let* ([difficulty 'easy]
         [size 3]
         [panel (new vertical-panel%
                     [parent master-panel])]
         [difficulty-msg (new message%
                              [parent panel]
                              [label "Difficulty Level: Easy"]
                              [auto-resize #t])]
         [set-difficulty! (λ (diff) (set! difficulty diff) (send difficulty-msg set-label (format "Difficulty Level: ~a" difficulty)))]
         [easy-button (new button%
                           [parent panel]
                           [label "Easy"]
                           [callback (λ (button event) (set-difficulty! 'easy))])]
         [medium-button (new button%
                             [parent panel]
                             [label "Medium"]
                             [callback (λ (button event) (set-difficulty! 'medium))])]
         [hard-button (new button%
                           [parent panel]
                           [label "Hard"]
                           [callback (λ (button event) (set-difficulty! 'hard))])]
         [evil-button (new button%
                           [parent panel]
                           [label "Evil"]
                           [callback (λ (button event) (set-difficulty! 'evil))])]
         [start-game-button (new button%
                                 [parent panel]
                                 [label "Start Game"]
                                 [callback (λ (button event)
                                             (set-options difficulty size)
                                             (handle-event 'start-game-button))])])
    panel))