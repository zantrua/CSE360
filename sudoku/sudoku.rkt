#lang racket

(require (file "game-panel.rkt")
         (file "login-panel.rkt")
         (file "invalid-login-panel.rkt")
         (file "menu-panel.rkt")
         (file "new-user-panel.rkt")
         (file "about-panel.rkt")
         (file "instructions-panel.rkt")
         
         (file "game-board.rkt")
         (file "state-machine.rkt")
         (file "save-file.rkt")
         
         (planet vyzo/crypto:2:3)
         (only-in file/sha1 bytes->hex-string)
         racket/gui)

; UI control state machine

(define game-state (make-state-machine (list (make-transition 'begin 'begin 'login-screen)
                                             
                                             (make-transition 'login-screen 'new-user-button 'new-user-screen)
                                             (make-transition 'login-screen 'valid-login 'menu-screen)
                                             (make-transition 'login-screen 'invalid-login 'invalid-login-screen)
                                             
                                             (make-transition 'invalid-login-screen 'new-user-button 'new-user-screen)
                                             (make-transition 'invalid-login-screen 'valid-login 'menu-screen)
                                             (make-transition 'invalid-login-screen 'invalid-login 'invalid-login-screen)
                                             
                                             (make-transition 'new-user-screen 'make-user-worked 'menu-screen)
                                             (make-transition 'new-user-screen 'make-user-failed 'new-user-screen)
                                             
                                             (make-transition 'menu-screen 'about-button 'about-screen)
                                             (make-transition 'menu-screen 'new-game-button 'game-screen)
                                             (make-transition 'menu-screen 'logout-button 'login-screen)
                                             
                                             (make-transition 'about-screen 'menu-button 'menu-screen)
                                             
                                             (make-transition 'game-screen 'game-menu-button 'menu-screen)
                                             (make-transition 'game-screen 'game-save-button 'save-screen))))

(define (handle-event event)
  (set! game-state (state-machine-get-next game-state event))
  (set-game-state (state-machine-get-state game-state)))

(define (set-game-state state)
  (clear-panels)
  (display (format "~a\n" state))
  (case state
    [(login-screen) (send login-panel show #t)]
    [(new-user-screen) (send new-user-panel show #t)]
    [(menu-screen) (send menu-panel show #t)]
    [(game-screen) (send game-panel show #t)]
    [(about-screen) (send about-panel show #t)]
    [(invalid-login-screen) (send invalid-login-panel show #t)]))

; Save system

(define save-file-path "save.dat")
(if (file-exists? save-file-path)
    (void)
    (with-output-to-file save-file-path (λ () (display '())))) ; Generate a new save file if there isn't one

; Login system

(define (password-hash pass)
  (bytes->hex-string (sha256 (string->bytes/utf-8 pass))))

(define (make-login name pass)
  #t)

(define (check-login name pass)
  (let* ([save-value (file->value save-file-path)]
         [matches (filter (λ (x) (eq? (save-file-user-get-name x) name)) save-value)]
         [pass-hash (password-hash pass)])
    (and (not (empty? matches))
         (eq? (first matches) pass-hash))))

; Top level window

(define (make-frame frame-size)
  (new frame%
       [label "Sudoku"]
       [min-width (+ frame-size (get-border-size))]
       [min-height (+ frame-size (get-border-size))]
       [stretchable-width #f]
       [stretchable-height #f]))

(define (init-frame frame game-panel)
  (let-values ([(frame-sx frame-sy) (send frame get-size)]
               [(game-sx game-sy) (send game-panel get-size)])
    (send frame min-width (+ frame-sx game-sx))
    (send frame min-height (+ frame-sy game-sy))))

(define (make-master-panel frame)
  (new panel%
       [parent frame]))

; Panels

(define frame-size 300)
(define frame (make-frame frame-size))
(define master-panel (make-master-panel frame))

(define login-panel (make-login-panel check-login master-panel handle-event))
(define new-user-panel (make-new-user-panel make-login master-panel handle-event))
(define menu-panel (make-menu-panel master-panel handle-event))
(define about-panel (make-about-panel master-panel handle-event))
(define game-panel (make-game-panel master-panel handle-event))
(define invalid-login-panel (make-invalid-login-panel check-login master-panel handle-event))
(define instructions-panel (make-instructions-panel master-panel handle-event))

(init-frame frame game-panel)

(define (clear-panels)
  (send login-panel show #f)
  (send new-user-panel show #f)
  (send menu-panel show #f)
  (send about-panel show #f)
  (send game-panel show #f)
  (send invalid-login-panel show #f))

(handle-event 'begin)
(send frame show #t)
