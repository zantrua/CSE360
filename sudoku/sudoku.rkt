#lang racket

(require (file "game-panel.rkt")
         (file "login-panel.rkt")
         
         (file "game-board.rkt")
         (file "state-machine.rkt")
         racket/gui)

(define game-state (make-state-machine (list (make-transition 'begin 'begin 'login-screen)
                                             
                                             (make-transition 'login-screen 'new-user 'new-user-screen)
                                             (make-transition 'login-screen 'valid-login 'menu-screen)
                                             (make-transition 'login-screen 'invalid-login 'invalid-login-screen)
                                             
                                             (make-transition 'invalid-login-screen 'okay-button 'login-screen)
                                             
                                             (make-transition 'new-user-screen 'new-user-made 'menu-screen)
                                             
                                             (make-transition 'menu-screen 'about-button 'about-screen)
                                             
                                             (make-transition 'about-screen 'back-button 'menu-screen)
                                             
                                             (make-transition 'menu-screen 'new-game-button 'game-screen)
                                             
                                             (make-transition 'game-screen 'game-menu-button 'save-or-menu-screen)
                                             (make-transition 'game-screen 'game-save-button 'save-screen))))

(define (handle-event event)
  (set! game-state (state-machine-get-next game-state event))
  (set-game-state (state-machine-get-state game-state)))

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

(define (make-new-user-panel master-panel handle-event)
  (new panel%
         [parent master-panel]))

(define (make-menu-panel master-panel handle-event)
  (let* ([menu-panel (new vertical-panel%
                          [parent master-panel])]
         [menu-new-game-button (new button%
                                    [parent menu-panel]
                                    [label "New Game"]
                                    [callback (λ (button event) (handle-event 'new-game-button))])])
    menu-panel))

(define frame-size 512)
(define frame (make-frame frame-size))
(define master-panel (make-master-panel frame))

(define login-panel (make-login-panel master-panel handle-event))
(define new-user-panel (make-new-user-panel master-panel handle-event))
(define menu-panel (make-menu-panel master-panel handle-event))
(define game-panel (make-game-panel master-panel handle-event))

(init-frame frame game-panel)

(define (clear-panels)
  (send login-panel show #f)
  (send new-user-panel show #f)
  (send menu-panel show #f)
  (send game-panel show #f))

(define (set-game-state state)
  (clear-panels)
  (display (format "~a\n" state))
  (case state
    [(login-screen) (send login-panel show #t)]
    [(new-user-screen) (send new-user-panel show #t)]
    [(menu-screen) (send menu-panel show #t)]
    [(game-screen) (send game-panel show #t)]))

(handle-event 'begin)
(send frame show #t)