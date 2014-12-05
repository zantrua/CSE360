#lang racket

(require (file "game-panel.rkt")
         (file "login-panel.rkt")
         (file "invalid-login-panel.rkt")
         (file "menu-panel.rkt")
         (file "new-user-panel.rkt")
         (file "about-panel.rkt")
         (file "instructions-panel.rkt")
         (file "options-panel.rkt")
         (file "save-panel.rkt")
         (file "load-panel.rkt")
         (file "scores-panel.rkt")
         (file "win-panel.rkt")
         
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
                                             (make-transition 'menu-screen 'new-game-button 'options-screen)
                                             (make-transition 'menu-screen 'load-button 'load-screen)
                                             (make-transition 'menu-screen 'instructions-button 'instructions-screen)
                                             (make-transition 'menu-screen 'scores-button 'scores-screen)
                                             (make-transition 'menu-screen 'logout-button 'login-screen)
                                             
                                             (make-transition 'scores-screen 'menu-button 'menu-screen)
                                             
                                             (make-transition 'about-screen 'menu-button 'menu-screen)
                                             
                                             (make-transition 'instructions-screen 'menu-button 'menu-screen)
                                             
                                             (make-transition 'options-screen 'start-game-button 'game-screen)
                                             
                                             (make-transition 'game-screen 'game-menu-button 'menu-screen)
                                             (make-transition 'game-screen 'game-save-button 'save-screen)
                                             (make-transition 'game-screen 'complete 'win-screen)
                                             
                                             (make-transition 'win-screen 'menu-button 'menu-screen)
                                             
                                             (make-transition 'save-screen 'saved 'game-screen)
                                             
                                             (make-transition 'load-screen 'menu-button 'menu-screen)
                                             (make-transition 'load-screen 'load-button 'game-screen))))

(define (handle-event event)
  (set! game-state (state-machine-get-next game-state event))
  (set-game-state (state-machine-get-state game-state)))

(define (set-game-state state)
  (clear-panels)
  (case state
    [(login-screen) (send login-panel show #t)]
    [(new-user-screen) (send new-user-panel show #t)]
    [(menu-screen) (send menu-panel show #t)]
    [(game-screen) (send game-panel show #t)]
    [(about-screen) (send about-panel show #t)]
    [(invalid-login-screen) (send invalid-login-panel show #t)]
    [(options-screen) (send options-panel show #t)]
    [(instructions-screen) (send instructions-panel show #t)]
    [(save-screen) (send save-panel show #t)]
    [(load-screen) (begin (load-games save-file-path user-name-get) (send load-panel show #t))]
    [(scores-screen) (begin (update-scores) (send scores-panel show #t))]
    [(win-screen) (send win-panel show #t)]))

; Save system

(define save-file-path "save.dat")
(define user-name "")
(if (file-exists? save-file-path)
    (void)
    (with-output-to-file save-file-path (λ () (write '())))) ; Generate a new save file if there isn't one

; Login system

(define (make-salt)
  (map (λ (x) (string-ref "0123456789abcdef" (random 16))) (range 64)))

(define (password-hash pass salt)
  (bytes->hex-string (sha256 (string->bytes/utf-8 (string-append pass salt)))))

(define (make-login name pass)
  (if (and (> (string-length name) 0)
           (> (string-length pass) 0))
      (let* ([save-value (file->value save-file-path)]
             [matches (filter (λ (x) (string=? (save-file-user-get-name x) name)) save-value)]
             [salt (make-salt)]
             [pass-hash (password-hash pass salt)])
        (if (empty? matches)
            (begin (with-output-to-file save-file-path (λ () (write (cons (make-save-file-user name pass-hash '()) save-value))) #:exists 'replace)
                   (set! user-name name)
                   (set-user-greeting name)
                   #t)
            #f))
      #f))

(define (check-login name pass)
  (let* ([save-value (file->value save-file-path)]
         [matches (filter (λ (x) (string=? (save-file-user-get-name x) name)) save-value)])
    (if (not (empty? matches))
        (let* ([user (first matches)]
               [salt (save-file-user-get-pass-salt user)]
               [pass-hash (password-hash pass salt)])
          (if (string=? (save-file-user-get-pass-hash user) pass-hash)
              (begin (set! user-name name)
                     (set-user-greeting name)
                     #t)
              #f))
        #f)))

(define (user-name-get)
  user-name)

(define save-options empty)
(define (set-save-options lst)
  (set! save-options lst))
(define (get-save-options)
  save-options)

; Top level window

(define (make-frame frame-size)
  (new frame%
       [label "Sudoku"]
       [min-width (+ frame-size (get-border-size))]
       [min-height (+ frame-size (get-border-size))]))

(define (make-master-panel frame)
  (new panel%
       [parent frame]))

; Panels

(define frame-size 0)
(define frame (make-frame frame-size))
(define master-panel (make-master-panel frame))

(define login-panel (make-login-panel check-login master-panel handle-event))
(define new-user-panel (make-new-user-panel make-login master-panel handle-event))
(define menu-panel (make-menu-panel user-name-get master-panel handle-event))
(define about-panel (make-about-panel master-panel handle-event))
(define game-panel (make-game-panel set-save-options set-score user-name-get master-panel handle-event))
(define invalid-login-panel (make-invalid-login-panel check-login master-panel handle-event))
(define instructions-panel (make-instructions-panel master-panel handle-event))
(define options-panel (make-options-panel set-options master-panel handle-event))
(define save-panel (make-save-panel save-file-path user-name-get get-save-options master-panel handle-event))
(define load-panel (make-load-panel load-game-options save-file-path user-name-get master-panel handle-event))
(define scores-panel (make-scores-panel master-panel handle-event))
(define win-panel (make-win-panel scores-file-path master-panel handle-event))

(define (clear-panels)
  (send login-panel show #f)
  (send new-user-panel show #f)
  (send menu-panel show #f)
  (send about-panel show #f)
  (send game-panel show #f)
  (send invalid-login-panel show #f)
  (send instructions-panel show #f)
  (send options-panel show #f)
  (send save-panel show #f)
  (send load-panel show #f)
  (send scores-panel show #f)
  (send win-panel show #f))

(handle-event 'begin)
(send frame show #t)
