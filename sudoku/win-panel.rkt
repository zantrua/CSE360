#lang racket

(require racket/gui)

(provide make-win-panel set-score)

(define win-message empty)
(define scores-file-path "")

(define (set-score score level user)
  (define (insert-score scores new-score)
    (let ([carry empty])
      (map (λ (score)
             (if (empty? carry)
                 (if (> (second new-score) (second score))
                     (begin (set! carry score)
                            new-score)
                     score)
                 (let ([old-carry carry])
                   (begin (set! carry score)
                          old-carry))))
           scores)))
  (send win-message set-label (format "Congratulations, you win!\nYour score was ~a." score))
  (let ([new-scores (let ([all-scores (file->value scores-file-path)])
                      (map (λ (line) (list (first line)
                                           (if (string=? (first line) level)
                                               (insert-score (rest line) (list user score))
                                               line))) all-scores))])
    (with-output-to-file scores-file-path (λ () (write new-scores)) #:exists 'replace)))

(define (make-win-panel scores-file-path-local master-panel handle-event)
  (let* ([panel (new vertical-panel%
                       [parent master-panel])]
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
