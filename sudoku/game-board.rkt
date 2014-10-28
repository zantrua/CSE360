#lang racket

(require (file "puzzle.rkt")
         (file "utility.rkt")
         (file "position.rkt")
         (file "rectangle.rkt")
         racket/gui)

(provide draw-puzzle handle-click get-border-size)

(define thick-pen (send the-pen-list find-or-create-pen "black" 3 'solid))
(define thin-pen (send the-pen-list find-or-create-pen "black" 1 'solid))
(define large-font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
(define small-font (send the-font-list find-or-create-font 10 'default 'normal 'normal))

; TODO: put this in the puzzle itself
(define click-rects empty)

(define (get-border-size)
  (send thick-pen get-width))

(define (draw-frame puzzle dc x y)
  (let-values ([(size-x size-y) (send dc get-size)])
    (define width (puzzle-width puzzle))
    (define square-size size-x)
    (define (get-pen x)
      (if (= 0 (modulo x (sqrt width)))
          thick-pen
          thin-pen))
    
    (send dc set-pen (get-pen x))
    (send dc draw-line 0 0 0 square-size)
    
    (send dc set-pen (get-pen (1+ x)))
    (send dc draw-line square-size 0 square-size square-size)
    
    (send dc set-pen (get-pen y))
    (send dc draw-line 0 0 square-size 0)
    
    (send dc set-pen (get-pen (1+ y)))
    (send dc draw-line 0 square-size square-size square-size)))

(define (draw-tile puzzle square-size x y)
  (let* ([bitmap (make-object bitmap% (ceiling square-size) (ceiling square-size))]
         [dc (send bitmap make-dc)]
         [value (tile-get-value (puzzle-ref puzzle (make-pos x y)))]
         [n (sqrt (puzzle-width puzzle))])
    (draw-frame puzzle dc x y)
    (if (= value 0)
        (begin
          (send dc set-font small-font)
          (for* ([i n][j n])
            (let*-values ([(value) (+ 1 i (* j n))]
                          [(text) (format "~a" value)]
                          [(text-width text-height descender ascender) (send dc get-text-extent text)]
                          [(text-x) (- (* (1+ i) (/ square-size (1+ n))) (/ text-width 2))]
                          [(text-y) (- (* (1+ j) (/ square-size (1+ n))) (/ text-height 2))]
                          [(rect) (cons (click-function-small puzzle value x y) (make-rectangle-corner-size (make-pos (+ (* x square-size) text-x)
                                                                                                                      (+ (* y square-size) text-y))
                                                                                                            (make-pos text-width
                                                                                                                      text-height)))])
              (set! click-rects (cons rect click-rects))
              (send dc draw-text text text-x text-y))))
        (begin
          (send dc set-font large-font)
          (let*-values ([(text) (format "~a" value)]
                        [(text-width text-height descender ascender) (send dc get-text-extent text)]
                        [(text-x) (- (/ square-size 2) (/ text-width 2))]
                        [(text-y) (- (/ square-size 2) (/ text-height 2))]
                        [(rect) (cons (click-function-large puzzle x y) (make-rectangle-corner-size (make-pos (+ (* x square-size) text-x)
                                                                                                              (+ (* y square-size) text-y))
                                                                                                    (make-pos text-width
                                                                                                              text-height)))])
            (set! click-rects (cons rect click-rects))
            (send dc draw-text text text-x text-y))))
    bitmap))

(define (draw-puzzle puzzle dc)
  (let-values ([(size-x size-y) (send dc get-size)])
    (define width (puzzle-width puzzle))
    (define square-size (/ size-x width))
    (set! click-rects empty)
    (for* ([x width][y width])
      (send dc
            draw-bitmap
            (draw-tile  puzzle square-size x y)
            (+ (/ (send thick-pen get-width) 2) (* x square-size))
            (+ (/ (send thick-pen get-width) 2) (* y square-size))))))

; Click handling functions
(define (click-function-small puzzle value x y)
  (let ([pos (make-pos x y)])
    (λ () (replace-puzzle-tile puzzle
                               pos
                               (make-tile value)))))

(define (click-function-large puzzle x y)
  (let ([pos (make-pos x y)])
    (λ () (replace-puzzle-tile puzzle
                               pos
                               (make-tile (if (not (tile-get-locked (puzzle-ref puzzle pos)))
                                              0
                                              (tile-get-value (puzzle-ref puzzle pos))))))))

(define (handle-click puzzle pos)
  (let ([click-rect (findf (λ (rect) (rectangle-contains? (cdr rect) pos)) click-rects)])
    (display click-rect)
    (if click-rect
        ((car click-rect))
        puzzle)))
