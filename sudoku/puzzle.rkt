#lang racket

(require (file "utility.rkt")
         (file "position.rkt")
         (file "partial-puzzle.rkt")
         (file "tile.rkt"))

(provide make-empty-puzzle make-puzzle puzzle-solved? puzzle-width puzzle-ref replace-puzzle-tile puzzle-print puzzle-unsolve)

; Makes an empty puzzle by filling the grid with empty tiles
(define (make-empty-puzzle (n 3))
  (define width (square n))
  (tile-function (λ (pos) (make-empty-tile)) width))

; Makes a puzzle using a recursive backtracking algorithm
(define (make-puzzle (n 3))
  (letrec ([width (square n)]
           [step  (λ (p pos)
                    (if (not pos)
                        (partial-puzzle->puzzle p)
                        (let* ([tile-pred-function (λ (f)
                                                     (tile-function (λ (in-pos)
                                                                      (if (pos=? pos in-pos)
                                                                          (f in-pos)
                                                                          (partial-puzzle-ref p in-pos))) width))]
                               [reset-tile (λ (p) (tile-pred-function (λ (pos) (make-partial-puzzle-tile n))))]
                               [clear-tile (λ (p) (tile-pred-function (λ (pos) (make-partial-puzzle-tile n 0 (partial-puzzle-ref-numbers p pos)))))]
                               [set-tile (λ (p) (tile-pred-function (λ (pos) (let* ([lst (partial-puzzle-ref-numbers p pos)]
                                                                                    [idx (random (length lst))]
                                                                                    [val (list-ref lst idx)])
                                                                               (make-partial-puzzle-tile n val (remove-index lst idx))))))])
                          (if (empty? (partial-puzzle-ref-numbers p pos))
                              (step (reset-tile p) (pos-backward pos width))
                              (let ([p (set-tile p)])
                                (if (puzzle-solved? (partial-puzzle->puzzle p))
                                    (step p (pos-forward pos width))
                                    (step (clear-tile p) pos)))))))])
    (step (make-partial-puzzle n) (make-pos 0 0))))

(define (puzzle-solved? p (exact #f))
  (let* ([width (puzzle-width p)]
         [check-set (λ (p proc)
                      (let* ([get-cell (λ (pos)
                                         (if (proc pos)
                                             (tile-get-value (puzzle-ref p pos))
                                             #f))]
                             [set (filter-map get-cell (cartesian-product (range width) (range width)))])
                        (if exact
                            (set=? (list->set set) (list->set (range 1 (1+ width))))
                            (= (length set) (length (remove-duplicates set (λ (a b) (and (= a b)
                                                                                         (not (= a 0))))))))))]
         [in-square? (λ (pos n)
                       (let* ([sqrt-width (sqrt width)]
                              [square-x (modulo n 3)]
                              [square-y (quotient n 3)]
                              [start-x (* sqrt-width square-x)]
                              [start-y (* sqrt-width square-y)])
                         (let-values ([(x y) (pos-get-values pos)])
                           (and (member x (range start-x (+ start-x sqrt-width)))
                                (member y (range start-y (+ start-y sqrt-width)))))))])
    (andmap (λ (proc) (check-set p proc))
            (append (map (λ (n) (λ (pos) (= (pos-get-x pos) n))) (range width))
                    (map (λ (n) (λ (pos) (= (pos-get-y pos) n))) (range width))
                    (map (λ (n) (λ (pos) (in-square? pos n))) (range width))))))

(define (puzzle-width p)
  (length p))

(define (puzzle-ref p pos)
  (let-values ([(x y) (pos-get-values pos)])
    (list-ref (list-ref p y) x)))

(define (replace-puzzle-tile p pos value)
  (tile-function (λ (pos-test) (if (pos=? pos pos-test)
                                   value
                                   (puzzle-ref p pos-test))) (puzzle-width p)))

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

(define (puzzle-unsolve p (part-to-remove 1/2))
  (letrec ([width (puzzle-width p)]
           [remove-step (λ (p steps)
                          (if (= steps 0)
                              p
                              (let ([rand-pos (make-pos (random width) (random width))])
                                (if (= (tile-get-value (puzzle-ref p rand-pos)) 0)
                                    (remove-step p steps)
                                    (remove-step (tile-function (λ (pos) (if (pos=? pos rand-pos)
                                                                             (make-empty-tile)
                                                                             (puzzle-ref p pos))) width) (1- steps))))))])
    (remove-step p (floor (* (square width) part-to-remove)))))
