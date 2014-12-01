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
                            [callback (λ (button event) (handle-event 'menu-button))])])
    (for ([line '("Objective: Fill in each square with a number 1-9."
                  "Each 3x3 square contains each number only once."
                  "Each row and column also contains the numbers 1-9 only once."
                  "Left-click to enter a number in a cell or to erase a guess,"
                  "right-click on each number to make notes,"
                  "right-click again to delete notes."
                  "When the puzzle is solved, your score will be shown."
                  "To save the game and resume it later"
                  "click the “save” button during gameplay."
                  "To load a game, visit the load screen and select the game to resume"
                  "then click the “load” button."
                  "To view high scores, click the “highscores” button on the Menu"
                  "Scores are displayed by difficulty."
                  "When solving any particular board"
                  "there may exist multiple solutions."
                  "Happy Hunting!")])
      (new message%
           [parent panel]
           [label line]))
    panel))
