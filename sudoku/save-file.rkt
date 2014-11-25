#lang racket

(require (file "puzzle.rkt"))

(provide 
 make-save-file
 make-save-file-user save-file-user-get-name save-file-user-get-pass-hash save-file-user-get-games
 make-save-file-game save-file-game-get-name save-file-game-get-level save-file-game-get-time
 save-file-game-get-hints save-file-game-get-wrong-moves save-file-game-get-board)

; Save-file top level

(define (make-save-file users)
  users)

; Users

(define (make-save-file-user name pass-hash games)
  (list name pass-hash games))

(define (save-file-user-get-name user)
  (first user))

(define (save-file-user-get-pass-hash user)
  (second user))

(define (save-file-user-get-games user)
  (third user))

; Games

(define (make-save-file-game name level time hints wrong-moves board)
  (list name level time hints wrong-moves board))

(define (save-file-game-get-name game)
  (first game))

(define (save-file-game-get-level game)
  (second game))

(define (save-file-game-get-time game)
  (third game))

(define (save-file-game-get-hints game)
  (fourth game))

(define (save-file-game-get-wrong-moves game)
  (fifth game))

(define (save-file-game-get-board game)
  (sixth game))
