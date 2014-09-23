#lang racket

(require (file "puzzle.rkt")
         (file "utility.rkt")
         (file "position.rkt")
         racket/gui)

(letrec ([frame-size 800]
         [n 3]
         [width (square n)]
         [gap (/ frame-size width)]
         [puzzle (puzzle-unsolve (make-puzzle n))]
         [thick-line-pen (send the-pen-list find-or-create-pen "black" 3 'solid)]
         [thin-line-pen (send the-pen-list find-or-create-pen "black" 1 'solid)]
         [large-font (send the-font-list find-or-create-font 20 'default 'normal 'normal)]
         [small-font (send the-font-list find-or-create-font 8 'default 'normal 'normal)]
         [draw-small-text (λ (dc x y) 
                            (send dc set-font small-font)
                            (for* ([i n][j n])
                                        (let*-values ([(text) (format "~a" (+ 1 i (* j n)))]
                                                      [(text-width text-height descender ascender) (send dc get-text-extent text)]
                                                      [(offset) (+ (/ gap 2) (/ (send thick-line-pen get-width) 2))]
                                                      [(text-x) (+ (* x gap) (* (/ gap (1+ n)) (1+ i)) (/ text-width -2))]
                                                      [(text-y) (+ (* y gap) (* (/ gap (1+ n)) (1+ j)) (/ text-height -2))])
                                          (send dc draw-text text text-x text-y))))]
         [draw-large-text (λ (dc value x y) (let*-values ([(text) (format "~a" value)]
                                                          [(text-width text-height descender ascender) (send dc get-text-extent text)]
                                                          [(offset) (+ (/ gap 2) (/ (send thick-line-pen get-width) 2))]
                                                          [(text-x) (+ offset (* x gap) (/ text-width -2))]
                                                          [(text-y) (+ offset (* y gap) (/ text-height -2))])
                                              (send dc set-font large-font)
                                              (send dc draw-text text text-x text-y)))]
         [sudoku-draw (λ (canvas dc)
                        (for ([i (1+ width)])
                          (send dc set-pen (if (= 0 (modulo i n))
                                               thick-line-pen
                                               thin-line-pen))
                          (let ([pos (+ (/ (send thick-line-pen get-width) 2) (* i gap))])
                            (send dc draw-line pos 0 pos frame-size)
                            (send dc draw-line 0 pos frame-size pos)))
                        (for* ([x width][y width])
                          (let ([value (puzzle-ref puzzle (make-pos x y))])
                            (if (= value 0)
                                (draw-small-text dc x y)
                                (draw-large-text dc value x y)))))]
         [sudoku-canvas% (let ([last-event-type ""])
                           (class canvas%
                             (define/override (on-event event)
                               (let ([event-type (send event get-event-type)]
                                     [mouse-x (send event get-x)]
                                     [mouse-y (send event get-y)])
                                 (void)))
                             (super-new)))]
         [frame
          (new frame%
               [label "Sudoku"]
               [min-width (+ (send thick-line-pen get-width) frame-size)]
               [min-height (+ (send thick-line-pen get-width) frame-size)])]
         [canvas (new sudoku-canvas%
                    [parent frame]
                    [paint-callback sudoku-draw])])
  (send frame show #t))
