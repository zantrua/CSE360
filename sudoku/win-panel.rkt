#lang racket

(require racket/gui)

(provide make-win-panel set-score)

(define win-message empty)
(define scores-file-path "")

(define (set-score score level user)
  (define (insert-score scores new-score)
    (define (get-score scores)
      (if (empty? scores)
          empty
          (let ([score (first scores)])
            (if (> (second new-score) (second score))
                (cons new-score scores)
                (cons score (get-score (rest scores)))))))
    (take (get-score scores) 10))
  (send win-message set-label (format "Congratulations, you win!\nYour score was ~a." score))
  (let ([new-scores (let ([all-scores (file->value scores-file-path)])
                      (map (λ (line) (if (string=? (first line) level)
                                         (cons (first line) (insert-score (rest line) (list user score)))
                                         line)) all-scores))])
    (with-output-to-file scores-file-path (λ () (write new-scores)) #:exists 'replace)))

(define (make-win-panel scores-file-path-local master-panel handle-event)
  (let* ([panel (new vertical-panel%
                     [parent master-panel]
                     [alignment '(center center)])]
         [menu-button (new button%
                           [parent panel]
                           [label "Menu"]
                           [callback (λ (button event) (handle-event 'menu-button))])]
         [message (new message%
                       [parent panel]
                       [label "Winner! "]
                       [auto-resize #t])])
    (set! win-message message)
    (set! scores-file-path scores-file-path-local)
    panel))
