#lang racket

(require (file "utility.rkt")
         (file "position.rkt")
         (file "partial-puzzle.rkt")
         (file "tile.rkt"))

(provide make-empty-puzzle make-puzzle puzzle-solved? puzzle-width puzzle-ref
         replace-puzzle-tile puzzle-print puzzle-unsolve puzzle-get-hint)

; Makes an empty puzzle by filling the grid with empty tiles
(define (make-empty-puzzle (n 3))
  (define width (square n))
  (tile-function (λ (pos) (make-empty-tile)) width))

; Makes a puzzle using a recursive backtracking algorithm
(define (solve-puzzle part-puzz)
  (letrec ([width (partial-puzzle-width part-puzz)]
           [n (sqrt width)]
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
                          (if (partial-puzzle-ref-locked p pos)
                              (step p (pos-forward pos width))
                              (if (empty? (partial-puzzle-ref-numbers p pos))
                                  (step (reset-tile p) (pos-backward pos width))
                                  (let ([p (set-tile p)])
                                    (if (puzzle-solved? (partial-puzzle->puzzle p))
                                        (step p (pos-forward pos width))
                                        (step (clear-tile p) pos))))))))])
    (step part-puzz (make-pos 0 0))))

(define (make-puzzle (n 3))
    (solve-puzzle (make-partial-puzzle n)))

(define (puzzle-solved? p (exact #f))
  (let* ([width (puzzle-width p)]
         [in-square? (λ (pos n)
                       (let* ([sqrt-width (sqrt width)]
                              [square-x (modulo n sqrt-width)]
                              [square-y (quotient n sqrt-width)]
                              [start-x (* sqrt-width square-x)]
                              [start-y (* sqrt-width square-y)])
                         (let-values ([(x y) (pos-get-values pos)])
                           (and (member x (range start-x (+ start-x sqrt-width)))
                                (member y (range start-y (+ start-y sqrt-width)))))))]
         [check-set (λ (p proc)
                      (let* ([get-cell (λ (pos)
                                         (if (proc pos)
                                             (tile-get-value (puzzle-ref p pos))
                                             #f))]
                             [set (filter-map get-cell (cartesian-product (range width) (range width)))])
                        (if exact
                            (set=? (list->set set) (list->set (range 1 (1+ width))))
                            (= (length set) (length (remove-duplicates set (λ (a b) (and (= a b)
                                                                                         (not (= a 0))))))))))])
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

(define (puzzle-get-hint p)
  (let* ([width (puzzle-width p)]
         [empty-squares (filter-map (λ (pos) (let ([tile (puzzle-ref p pos)])
                                               (if (= (tile-get-value tile) 0)
                                                   pos
                                                   #f))) (cartesian-product (range width) (range width)))]
         [get-possible (λ (p pos) (filter-map (λ (val) (if (puzzle-solved? (replace-puzzle-tile p pos (make-tile val)))
                                                           val
                                                           #f)) (shuffle (range 1 (+ 1 width)))))])
    (let ([possible (filter-map (λ (pos) (let ([possible (get-possible p pos)])
                                           (if (empty? possible)
                                               #f
                                               (list pos (first possible))))) (shuffle empty-squares))])
      (first possible))))
