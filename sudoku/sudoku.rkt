#lang racket

(require (file "puzzle.rkt")
         (file "utility.rkt")
         (file "position.rkt")
         (file "rectangle.rkt")
         racket/gui)


(define thick-pen (send the-pen-list find-or-create-pen "black" 3 'solid))
(define thin-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
(define large-font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
(define small-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))

(define n 3)
(define width (square n))
(define frame-size (* 100 width))
(define square-size (/ frame-size width))

(define click-rects empty)

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

(define (set-puzzle-tile! x y value)
  (write (format "set ~a, ~a to ~a" x y value)))

(define (click-function-small value x y)
  (λ (click-type) (if (eq? click-type 'left-down)
                      (set-puzzle-tile! x y value)
                      (void))))

(define (draw-tile x y)
  (let* ([bitmap (make-object bitmap% (ceiling square-size) (ceiling square-size))]
         [dc (send bitmap make-dc)]
         [value (puzzle-ref puzzle (make-pos x y))])
    (draw-frame dc x y)
    (if (= value 0)
        (begin
          (send dc set-font small-font)
          (for* ([i n][j n])
            (let*-values ([(value) (+ 1 i (* j n))]
                          [(text) (format "~a" value)]
                          [(text-width text-height descender ascender) (send dc get-text-extent text)]
                          [(text-x) (- (* (1+ i) (/ square-size (1+ n))) (/ text-width 2))]
                          [(text-y) (- (* (1+ j) (/ square-size (1+ n))) (/ text-height 2))]
                          [(rect) (cons (click-function-small value x y) (make-rectangle (make-pos (+ (* x square-size) text-x)
                                                                                                   (+ (* y square-size) text-y))
                                                                                         (make-pos (+ (* x square-size) text-x text-width)
                                                                                                   (+ (* y square-size) text-y text-height))))])
              (set! click-rects (cons rect click-rects))
              (send dc draw-text text text-x text-y))))
        (let*-values ([(text) (format "~a" value)]
                      [(text-width text-height descender ascender) (send dc get-text-extent text)]
                      [(text-x) (- (/ square-size 2) text-width)]
                      [(text-y) (- (/ square-size 2) text-height)])
          (send dc set-font large-font)
          (send dc draw-text text text-x text-y)))
    bitmap))

(define (draw-values dc)
  (set! click-rects empty)
  (for* ([x width][y width])
    (send dc
          draw-bitmap
          (draw-tile x y)
          (+ (/ (send thick-pen get-width) 2) (* x square-size))
          (+ (/ (send thick-pen get-width) 2) (* y square-size)))))

(define sudoku-canvas% (let ([last-event-type ""])
                         (class canvas%
                           (define/override (on-event event)
                             (let* ([event-type (send event get-event-type)]
                                    [mouse-x (send event get-x)]
                                    [mouse-y (send event get-y)]
                                    [mouse-pos (make-pos mouse-x mouse-y)])
                               (if (or (eq? event-type 'left-down)
                                       (eq? event-type 'right-down))
                                   (let ([click-rect (findf (λ (rect) (rectangle-contains? (cdr rect) mouse-pos)) click-rects)])
                                     (if click-rect
                                         ((car click-rect) event-type)
                                         (void)))
                                   (void))))
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
