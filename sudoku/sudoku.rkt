#lang racket
(require (file "utility.rkt"))

(provide make-puzzle puzzle-width puzzle-ref puzzle-print)

(define (make-puzzle (n 3))
  (define width (square n))
  
  (build-list width
              (λ (y) (build-list width
                                 (λ (x) 0)))))

(define (puzzle-width p) (length p))

(define (puzzle-ref p x y) (list-ref (list-ref p y) x))

(define (puzzle-print p)
  (let* ([width (puzzle-width p)]
         [display-line (λ (last-line) (display (format "~%~a+~%~a"
                                                       (foldl string-append "" (build-list width (λ (n) "+---")))
                                                       (if last-line "" "|"))))])
    (for ([y width])
      (display-line #f)
      (for ([x width])
        (display (format " ~a |" (puzzle-ref p x y)))))
    (display-line #t)))
