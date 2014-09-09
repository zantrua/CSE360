#lang racket
(require (file "utility.rkt"))
(provide make-pos pos-get-x pos-get-y pos-get-values pos=? pos-backward pos-forward pos-print tile-function)

(define (make-pos x y)
  (cons x y))

(define (pos-get-x p)
  (car p))

(define (pos-get-y p)
  (cdr p))

(define (pos-get-values p)
  (values (pos-get-x p) (pos-get-y p)))

(define (pos=? a b)
  (let-values ([(ax ay) (pos-get-values a)]
               [(bx by) (pos-get-values b)])
    (and (= ax bx)
         (= ay by))))

(define (pos-backward p width)
  (let-values ([(x y) (pos-get-values p)])
    (if (= x 0)
        (if (= y 0)
            (error "Backed off the board")
            (make-pos (1- width) (1- y)))
        (make-pos (1- x) y))))

(define (pos-forward p width)
  (let-values ([(x y) (pos-get-values p)])
    (if (= x (1- width))
        (if (= y (1- width))
            #f
            (make-pos 0 (1+ y)))
        (make-pos (1+ x) y))))
  
(define (pos-print p)
  (display (format "(~a, ~a)" (pos-get-x p) (pos-get-y p))))

(define (tile-function f width)
  (build-list width
              (λ (y) (build-list width
                                 (λ (x) (f (make-pos x y)))))))