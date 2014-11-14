#lang racket

(require (file "puzzle.rkt"))

(provide 
 make-save-file
 make-save-file-user
 make-save-file-game)

; A save file is as follows
; save-file = (user1 user2 ...)
; user = (name pass-hash (game1 game2 ...))
; game = (level time hints wrong-moves board)

(define (make-save-file users)
  users)

(define (make-save-file-user name pass-hash games)
  (list name pass-hash games))

(define (make-save-file-game level time hints wrong-moves board)
  (list level time hints wrong-moves board))
