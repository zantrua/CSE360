#lang racket

(require (file "save-file.rkt")
         racket/gui)

(provide make-save-panel)

(define (make-save-panel save-file-path user-name-get options-get master-panel handle-event)
  (let* ([panel (new vertical-panel% [parent master-panel])]
         [name (new text-field% [parent panel] [label "Save name"])]
         [msg (new message% [parent panel][label ""])]
         [btn (new button%
                   [parent panel]
                   [label "Save"]
                   [callback (λ (button event) 
                               (let* ([options (options-get)]
                                      [game (make-save-file-game (send name get-value)
                                                                 (first options)
                                                                 (second options)
                                                                 (third options)
                                                                 (fourth options)
                                                                 (fifth options))]
                                      [save-state (file->value save-file-path)]
                                      [save-new (map (λ (user)
                                                       (if (string=? (user-name-get) (save-file-user-get-name user))
                                                           (let ([games (save-file-user-get-games user)])
                                                             (if (ormap identity (map (λ (game) (string=? (save-file-game-get-name game) name)) games))
                                                                 (begin (send msg set-label "Save name already used")
                                                                        #f)
                                                                 (make-save-file-user (save-file-user-get-name user)
                                                                                      (save-file-user-get-pass-hash user)
                                                                                      (cons game games))))
                                                           user)) save-state)])
                                 (write save-new)
                                 (if (andmap identity save-new)
                                     (with-output-to-file save-file-path (λ () (write save-new)) #:exists 'replace)
                                     (void))))])])
    panel))