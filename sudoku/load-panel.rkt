#lang racket

(require (file "save-file.rkt")
         racket/gui)

(provide make-load-panel load-games)

(define game-list empty)

(define (load-games save-file-path get-user-name)
  (let* ([file-data (file->value save-file-path)]
         [user (filter (λ (user) (string=? (get-user-name) (save-file-user-get-name user))) file-data)]
         [game-names (map (λ (game) (save-file-game-get-name game)) (save-file-user-get-games (first user)))])
    (send game-list set game-names)))

(define (make-load-panel load-game-options save-file-path get-user-name master-panel handle-event)
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
                                       (let* ([game-num (first (send game-list-inner get-selections))]
                                              [file-data (file->value save-file-path)]
                                              [user (first (filter (λ (user) (string=? (get-user-name) (save-file-user-get-name user))) file-data))]
                                              [game (list-ref (save-file-user-get-games user) game-num)])
                                         (load-game-options (save-file-game-get-level game)
                                                            (save-file-game-get-time game)
                                                            (save-file-game-get-hints game)
                                                            (save-file-game-get-wrong-moves game)
                                                            (save-file-game-get-board game))
                                         (handle-event 'load-button)))])]
         [menu-button (new button%
                           [parent panel]
                           [label "Menu"]
                           [callback (λ (button event) (handle-event 'menu-button))])])
    (set! game-list game-list-inner)
    panel))
