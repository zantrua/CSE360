#lang racket

(require (file "puzzle.rkt"))

(provide 
 make-save-file
 make-save-file-user save-file-user-get-name save-file-user-get-pass-hash save-file-user-get-games
 make-save-file-game)

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

(define (make-save-file-game level time hints wrong-moves board)
  (list level time hints wrong-moves board))
