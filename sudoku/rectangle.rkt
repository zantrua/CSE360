#lang racket

(require (file "position.rkt"))
(provide make-rectangle-center-size make-rectangle rectangle-get-corners rectangle-contains?)

(define (make-rectangle-center-size center size)
  (let-values ([(center-x center-y) (pos-get-values center)]
               [(size-x size-y) (pos-get-values size)])
    (make-rectangle (make-pos (- center-x (/ size-x 2))
                              (- center-y (/ size-y 2)))
                    (make-pos (+ center-x (/ size-x 2))
                              (+ center-y (/ size-y 2))))))

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
               