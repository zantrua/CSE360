#lang racket

(require racket/gui)

(provide make-about-panel)

(define (make-about-panel master-panel handle-event)
  (let* ([panel (new vertical-panel%
                           [parent master-panel]
                           [alignment '(center center)])]
         [about-button (new button%
                            [parent panel]
                            [label "Menu"]
                            [callback (λ (button event) (handle-event 'menu-button))])])
    (for ([line '("Sudoku is a game of logic where the objective is to combine"
                  "the numbers 1-9 in a particular order inside a puzzle."
                  "A puzzle was first published in 1979 by Howard Garns"
                  "in a magazine and has since taken off."
                  "First becoming popular in Japan in the late 80’s,"
                  "sudoku became an international hit by 2005."
                  "As a staple in most modern newspapers, this classic paper game"
                  "has made its way to your computer screen with this software."
                  "Can you defeat our most difficult puzzles? We dare you to try ;)")])
      (new message%
           [parent panel]
           [label line]))
    panel))
