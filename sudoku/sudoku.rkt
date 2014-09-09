#lang racket

(require (file "utility.rkt")
         (file "position.rkt"))

(provide make-puzzle make-empty-puzzle puzzle-width puzzle-ref puzzle-print)

(define (make-empty-puzzle (n 3))
  (define width (square n))
  
  (build-list width
              (λ (y) (build-list width
                                 (λ (x) 0)))))

(define (make-puzzle (n 3))
  (define width (square n))
  (define (step p pos)
    (if (not pos)
        (partial-puzzle->puzzle p)
        (void)))
  (void))
         

(define (puzzle-width p)
  (length p))

(define (puzzle-ref p pos)
  (let-values ([(x y) (pos-get-values pos)])
    (list-ref (list-ref p y) x)))

(define (puzzle-print p)
  (let* ([width (puzzle-width p)]
         [display-line (λ (last-line) (display (format "~%~a+~%~a"
                                                       (foldl string-append "" (build-list width (λ (n) "+---")))
                                                       (if last-line "" "|"))))])
    (for ([y width])
      (display-line #f)
      (for ([x width])
        (display (format " ~a |" (puzzle-ref p (make-pos x y))))))
    (display-line #t)))
