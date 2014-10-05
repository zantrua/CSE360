#lang racket

(require (file "position.rkt"))
(provide make-rectangle-corner-size make-rectangle rectangle-get-corners rectangle-contains?)

(define (make-rectangle-corner-size corner size)
    (make-rectangle corner
                    (pos+ corner size)))

(define (make-rectangle min max)
  (cons min max))

(define (rectangle-get-corners rect)
  (values (car rect) (cdr rect)))

(define (rectangle-contains? rect pos)
  (let*-values ([(min max) (rectangle-get-corners rect)]
                [(min-x min-y) (pos-get-values min)]
                [(max-x max-y) (pos-get-values max)]
                [(pos-x pos-y) (pos-get-values pos)])
    (and (>= pos-x min-x)
         (<= pos-x max-x)
         (>= pos-y min-y)
         (<= pos-y max-y))))
               