#lang racket

(require (file "puzzle.rkt")
         racket/gui)

(letrec ([sudoku-canvas%
          (let ([last-event-type ""])
            (class canvas%
              (define/override (on-event event)
                (let ([event-type (send event get-event-type)]
                      [mouse-x (send event get-x)]
                      [mouse-y (send event get-y)])
                  (case event-type
                    ((left-down left-up right-down right-up)
                     (send msg set-label (format "Mouse: ~a (~a, ~a)" event-type mouse-x mouse-y))
                     (set! last-event-type event-type))
                    ((motion) (send msg set-label (format "Mouse: ~a (~a, ~a)" last-event-type mouse-x mouse-y))))))
            (super-new)))]
         [frame
          (new frame%
               [label "Sudoku"]
               [width 512]
               [height 512])]
         [msg (new message% [parent frame]
                   [label "No events so far..."]
                   [min-width 500])]
         [canvas (new sudoku-canvas% [parent frame])])
  (send frame show #t))