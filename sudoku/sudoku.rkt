#lang racket

(require (file "puzzle.rkt")
         (file "utility.rkt")
         (file "position.rkt")
         racket/gui)


(define thick-pen (send the-pen-list find-or-create-pen "black" 3 'solid))
(define thin-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
(define large-font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
(define small-font (send the-font-list find-or-create-font 8 'default 'normal 'normal))

(define frame-size 800)
(define n 3)
(define width (square n))
(define square-size (/ frame-size width))

(define puzzle (puzzle-unsolve (make-puzzle n)))

(define (get-pen x)
  (if (= 0 (modulo x n))
      thick-pen
      thin-pen))

(define (draw-frame dc x y)
  (send dc set-pen (get-pen x))
  (send dc draw-line 0 0 0 square-size)
  
  (send dc set-pen (get-pen (1+ x)))
  (send dc draw-line square-size 0 square-size square-size)
  
  (send dc set-pen (get-pen y))
  (send dc draw-line 0 0 square-size 0)
  
  (send dc set-pen (get-pen (1+ y)))
  (send dc draw-line 0 square-size square-size square-size))

(define (draw-tile x y)
  (let* ([bitmap (make-object bitmap% (ceiling square-size) (ceiling square-size))]
         [dc (send bitmap make-dc)]
         [value (puzzle-ref puzzle (make-pos x y))]
         [text (format "~a" value)])
    (let*-values ([(text-width text-height descender ascender) (send dc get-text-extent text)]
                  [(text-x) (- (/ square-size 2) (/ text-width 2))]
                  [(text-y) (- (/ square-size 2) (/ text-height 2))])
      (send dc set-font large-font)
      (send dc draw-text text text-x text-y)
      (draw-frame dc x y))
    bitmap))

(define (draw-values dc)
  (for* ([x width][y width])
    (send dc draw-bitmap (draw-tile x y) (* x square-size) (* y square-size))))

(define sudoku-canvas% (let ([last-event-type ""])
                         (class canvas%
                           (define/override (on-event event)
                             (let ([event-type (send event get-event-type)]
                                   [mouse-x (send event get-x)]
                                   [mouse-y (send event get-y)])
                               (void)))
                           (super-new))))

(define frame
  (new frame%
       [label "Sudoku"]
       [min-width (+ (send thick-pen get-width) frame-size)]
       [min-height (+ (send thick-pen get-width) frame-size)]))

(define canvas (new sudoku-canvas%
                    [parent frame]
                    [paint-callback (λ (canvas dc) (draw-values dc))]))

(send frame show #t)
