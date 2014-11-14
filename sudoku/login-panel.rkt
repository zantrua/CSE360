#lang racket

(require racket/gui)

(provide make-login-panel)

(define save-file-path "save.dat")
(if (file-exists? save-file-path)
    (void)
    (with-output-to-file save-file-path (λ () (display '()))))

(define (make-login-panel master-panel handle-event)
  (let* ([login-panel (new vertical-panel%
                           [parent master-panel]
                           [alignment '(center center)])]
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
         [check-login (λ (save user pass)
                        #t)]
         [login-login-button (new button%
                                  [parent login-panel]
                                  [label "Login"]
                                  [callback (λ (button event)
                                              (let ([user-name (send login-name get-value)]
                                                    [password (send login-pass get-value)]
                                                    [save-state (file->value save-file-path)])
                                                (if (check-login save-state user-name password)
                                                    (handle-event 'valid-login)
                                                    (handle-event 'invalid-login))))])]
         [login-new-user-button (new button%
                                     [parent login-panel]
                                     [label "New User"])])
    login-panel))
