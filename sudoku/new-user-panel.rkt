#lang racket

(require racket/gui)

(provide make-new-user-panel)

(define (make-new-user-panel make-login master-panel handle-event)
  (let* ([new-user-panel (new vertical-panel%
                              [parent master-panel])]
         [user-name (new text-field%
                         [parent new-user-panel]
                         [label "Username"]
                         [stretchable-width #f]
                         [min-width 200])]
         [password (new text-field%
                        [parent new-user-panel]
                        [label "Password"]
                          [stretchable-width #f]
                        [min-width 200])]
         [make-user-button (new button%
                                [parent new-user-panel]
                                [label "Make user"]
                                [callback (λ (button event) (let ([user-name (send user-name get-value)]
                                                                  [password (send password get-value)])
                                                              (if (make-login user-name password)
                                                                  (handle-event 'make-user-worked)
                                                                  (handle-event 'make-user-failed))))])])
    new-user-panel))
