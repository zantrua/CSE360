#lang racket

(require (file "puzzle.rkt")
         (file "utility.rkt")
         (file "position.rkt")
         racket/gui)

(letrec ([frame-size 800]
         [puzzle (make-puzzle)]
         [thick-line-pen (send the-pen-list find-or-create-pen "black" 3 'solid)]
         [thin-line-pen (send the-pen-list find-or-create-pen "black" 1 'solid)]
         [sudoku-draw (λ (canvas dc)
                        (let* ([width (puzzle-width puzzle)]
                               [gap (/ frame-size width)]
                               [n (sqrt width)])
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
                                  (void)
                                  (let*-values ([(text) (format "~a" value)]
                                                [(text-width text-height descender ascender) (send dc get-text-extent text)]
                                                [(offset) (+ (/ gap 2) (/ (send thick-line-pen get-width) 2))]
                                                [(text-x) (+ offset (* x gap) (/ text-width -2))]
                                                [(text-y) (+ offset (* y gap) (/ text-height -2))])
                                    (send dc draw-text text text-x text-y)))))))]
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
