#lang racket

(require (file "puzzle.rkt")
         (file "utility.rkt")
         (file "position.rkt")
         racket/gui)

(define frame-size 800)

(define puzzle (make-puzzle))

(define thick-line-pen (send the-pen-list find-or-create-pen "black" 3 'solid))
(define thin-line-pen (send the-pen-list find-or-create-pen "black" 1 'solid))

(define (sudoku-draw canvas dc)
  (define width (puzzle-width puzzle))
  
  (define gap (/ frame-size width))
  
  (define n (sqrt width))
  
  (for ([i (1+ width)])
    (send dc set-pen (if (= 0 (modulo i n))
                         thick-line-pen
                         thin-line-pen))
    (let ([pos (+ (/ (send thick-line-pen get-width) 2) (* i gap))])
      (send dc draw-line pos 0 pos frame-size)
      (send dc draw-line 0 pos frame-size pos)))
  (for* ([x width][y width])
    (let*-values ([(text) (format "~a" (puzzle-ref puzzle (make-pos x y)))]
                  [(text-width text-height descender ascender) (send dc get-text-extent text)]
                  [(text-x) (+ (/ (send thick-line-pen get-width) 2) (* x gap) (/ gap 2) (- (/ text-width 2)))]
                  [(text-y) (+ (/ (send thick-line-pen get-width) 2) (* y gap) (/ gap 2) (- (/ text-height 2)))])
      (send dc draw-text text text-x text-y))))

(define sudoku-canvas%
  (let ([last-event-type ""])
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
       [min-width (+ (send thick-line-pen get-width) frame-size)]
       [min-height (+ (send thick-line-pen get-width) frame-size)]))

(define canvas (new sudoku-canvas%
                    [parent frame]
                    [paint-callback sudoku-draw]))

(send frame show #t)

(let-values ([(x y) (send frame get-client-size)])
  (display (format "~a, ~a" x y)))