#lang racket
(require (file "utility.rkt"))
(provide make-pos pos-get-x pos-get-y pos=? pos-backward pos-forward pos-print)

(define (make-pos x y)
  (cons x y))

(define (pos-get-x p)
  (car p))

(define (pos-get-y p)
  (cdr p))

(define (pos=? a b)
  (let ([ax (pos-get-x a)]
        [ay (pos-get-y a)]
        [bx (pos-get-x b)]
        [by (pos-get-y b)])
    (and (= ax bx)
         (= ay by))))

(define (pos-backward p width)
  (let ([x (pos-get-x p)]
        [y (pos-get-y p)])
  (if (= x 0)
      (if (= y 0)
          (error "Backed off the board")
          (make-pos (1- width) (1- y)))
      (make-pos (1- x) y))))

(define (pos-forward p width)
  (let ([x (pos-get-x p)]
        [y (pos-get-y p)])
    (if (= x (1- width))
        (if (= y (1- width))
            #f
            (make-pos 0 (1+ y)))
        (make-pos (1+ x) y))))
  
(define (pos-print p)
  (display (format "(~a, ~a)" (pos-get-x p) (pos-get-y p))))
