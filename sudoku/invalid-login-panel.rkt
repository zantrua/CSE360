#lang racket

(require racket/gui)

(provide make-invalid-login-panel)

(define (make-invalid-login-panel check-login master-panel handle-event)
  (let* ([login-panel (new vertical-panel%
                           [parent master-panel]
                           [alignment '(center center)])]
         [invalid-login-msg (new message%
                                 [parent login-panel]
                                 [label "Incorrect login"])]
         [login-name (new text-field%
                          [parent login-panel]
                          [label "Username"]
                          [stretchable-width #f]
                          [min-width 200])]
         [login-pass (new text-field%
                          [parent login-panel]
                          [label "Password"]
                          [stretchable-width #f]
                          [min-width 200])]
         [login-login-button (new button%
                                  [parent login-panel]
                                  [label "Login"]
                                  [callback (λ (button event)
                                              (let ([user-name (send login-name get-value)]
                                                    [password (send login-pass get-value)])
                                                (if (check-login user-name password)
                                                    (handle-event 'valid-login)
                                                    (handle-event 'invalid-login))))])]
         [login-new-user-button (new button%
                                     [parent login-panel]
                                     [label "New User"])])
    login-panel))
