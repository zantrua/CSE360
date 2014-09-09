#lang racket

(require (file "utility.rkt")
         (file "position.rkt")
         (file "partial-puzzle.rkt"))

(provide make-empty-puzzle make-puzzle puzzle-solved? puzzle-width puzzle-ref puzzle-print)

(define (make-empty-puzzle (n 3))
  (define width (square n))
  (tile-function (λ (x y) 0) width))

(define (make-puzzle (n 3))
  (define width (square n))
  (void))

(define (puzzle-solved? p (exact #f))
  (define width (puzzle-width p))
  
  (define (check-set p proc)
    (define (get-cell pos)
      (if (proc pos)
          (puzzle-ref p pos)
          #f))
    (define set (filter-map get-cell (cartesian-product (range width) (range width))))
    (if exact
        (set=? (list->set set) (list->set (range 1 (1+ width))))
        (= (length set) (length (remove-duplicates set (λ (a b) (and (= a b)
                                                                     (not (= a 0)))))))))
  
  (define (in-square? pos n)
    (let* ([sqrt-width (sqrt width)]
           [square-x (modulo n 3)]
           [square-y (quotient n 3)]
           [start-x (* sqrt-width square-x)]
           [start-y (* sqrt-width square-y)])
      (let-values ([(x y) (pos-get-values pos)])
        (and (member x (range start-x (+ start-x sqrt-width)))
             (member y (range start-y (+ start-y sqrt-width)))))))
  
  (andmap (λ (proc) (check-set p proc))
          (append (map (λ (n) (λ (pos) (= (pos-get-x pos) n))) (range width))
                  (map (λ (n) (λ (pos) (= (pos-get-y pos) n))) (range width))
                  (map (λ (n) (λ (pos) (in-square? pos n))) (range width)))))

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
