#lang racket

(require (planet joskoot/planet-fmt:1:10/fmt)
         racket/gui)

(provide make-scores-panel scores-file-path update-scores)

(define difficulties '("easy" "medium" "hard" "evil"))

(define scores-file-path "scores.dat")
(if (file-exists? scores-file-path)
    (void)
    (with-output-to-file scores-file-path (λ () (write (let ([score-list (make-list 10 '("Nobody" 0))])
                                                         (map cons difficulties (make-list 4 score-list)))))))

(define (get-scores level)
  (let* ([all-scores (file->value scores-file-path)]
         [scores (filter (λ (line) (string=? (first line) level)) all-scores)])
    (display (first scores))
    (first scores)))

(define (draw-scores msgs scores)
  (error-print-width 10)
  (for ([i 10])
    (let ([score (list-ref scores (+ 1 i))])
      (send (list-ref msgs i) set-label ((fmt "'#'L4D C16D C16D") (+ 1 i) (first score) (second score))))))

(define msgs-update empty)
(define (update-scores)
  (draw-scores msgs-update (get-scores (first difficulties))))

(define (make-scores-panel master-panel handle-event)
  (letrec ([panel (new vertical-panel%
                       [parent master-panel]
                       [alignment '(center center)])]
           [menu-button (new button%
                             [parent panel]
                             [label "Menu"]
                             [callback (λ (button event) (handle-event 'menu-button))])]
           [score-tabs (new tab-panel%
                            [parent panel]
                            [choices difficulties]
                            [callback (λ (tab-panel event)
                                        (let ([selection (send tab-panel get-selection)])
                                          (draw-scores msgs (get-scores (list-ref difficulties selection)))))])]
           [msgs (map (λ (x) (new message%
                                           [parent score-tabs]
                                           [label ""]
                                           [auto-resize #t])) (range 10))])
    (set! msgs-update msgs)
    (draw-scores msgs (get-scores (first difficulties)))
    panel))
